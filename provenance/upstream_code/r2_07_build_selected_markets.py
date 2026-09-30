"""Separate R2 duplicate lookup and market reconstruction; preserves original files.
Latest same-period candidates are a sensitivity policy, not proof of the exact PZ filing.
Unresolved conflicting grants make public HHI unknown, not inactive.
"""
import csv
import math
from pathlib import Path
from collections import defaultdict, Counter
from datetime import datetime

ROOT=Path(__file__).resolve().parents[2]
PERIOD=ROOT/'output/revisions/NVSQ_R2_2026-09/period_match_20260915_212219'
OUT=ROOT/'output/revisions/NVSQ_R2_2026-09'/('selected_markets_'+datetime.now().strftime('%Y%m%d_%H%M%S'))
OUT.mkdir(parents=True,exist_ok=False)

def read(path):
    with path.open(encoding='utf-8-sig',newline='') as f: yield from csv.DictReader(f)
def ein(v): return v.replace('EIN-','').replace('-','').zfill(9)
def amount(v): return None if v in ('','NA',None) else float(v)
def write(name,data):
    with (OUT/name).open('w',encoding='utf-8',newline='') as f:
        w=csv.DictWriter(f,fieldnames=list(data[0]));w.writeheader();w.writerows(data)

def main():
    policy={}
    for r in read(PERIOD/'period_match_candidates.csv'):
        k=(r['EIN'],r['TAXYEAR']);r['selected_raw_grant']=r['grant_value']
        if r['status'].startswith('period_matched'):
            r['policy']='use_period_matched_value_or_latest_candidate'
        elif r['prior_conflicting']=='False':
            r['policy']='retain_common_amount_period_unverified'
            r['selected_raw_grant']=None
        else:
            r['policy']='unresolved_do_not_impute';r['selected_raw_grant']=None
        policy[k]=r
    # Identical raw amounts can be retained without selecting a specific mismatched filing.
    for r in read(PERIOD/'unresolved_source_records.csv'):
        p=policy[r['EIN'],r['TAXYEAR']]
        if p['policy']=='retain_common_amount_period_unverified':
            v=r['F9_08_REV_CONTR_GOVT_GRANT']
            if p['selected_raw_grant'] is not None and p['selected_raw_grant']!=v:
                raise ValueError('Unexpected conflicting common amount')
            p['selected_raw_grant']=v
    write('duplicate_grant_lookup.csv',list(policy.values()))
    seen=set();markets={};affected=[];counts=Counter();nrows=0
    print('Streaming original organization rows',flush=True)
    for r in read(ROOT/'data/processed/pz_constituency_measures.csv'):
        if not 2012<=int(r['TAXYEAR'])<=2021: continue
        nrows+=1;k=(ein(r['EIN']),r['TAXYEAR']);p=policy.get(k)
        if p:
            if k in seen: counts['removed_repeat_rows']+=1;continue
            seen.add(k)
        mk=(r['CBSA'],r['ntee_broad_clean'],r['TAXYEAR'])
        m=markets.setdefault(mk,dict(n_orgs=0,private=0.,public=0.,client=0.,
            private_sq=0.,public_sq=0.,client_sq=0.,unresolved_grant_orgs=0,period_unverified_orgs=0))
        private=float(r['private_contributions']);client=float(r['client_services']);public=float(r['public_grants'])
        if p:
            counts[p['policy']]+=1
            if p['policy']=='unresolved_do_not_impute':
                public=None;m['unresolved_grant_orgs']+=1
                affected.append(dict(EIN=k[0],TAXYEAR=k[1],CBSA=mk[0],ntee_broad_clean=mk[1],status=p['status']))
            else:
                raw=amount(p['selected_raw_grant']);public=max(0,raw if raw is not None else 0)
                if p['policy']=='retain_common_amount_period_unverified':m['period_unverified_orgs']+=1
        m['n_orgs']+=1
        for side,v in [('private',private),('public',public),('client',client)]:
            if v is not None:
                if not math.isfinite(v) or v<0:raise ValueError('Invalid constructed revenue')
                m[side]+=v;m[side+'_sq']+=v*v
        if nrows%1000000==0:print(nrows,'source rows',flush=True)
    assert len(seen)==len(policy)
    assert counts['removed_repeat_rows']==47168
    output=[]
    for mk,m in markets.items():
        x=dict(CBSA=mk[0],ntee_broad_clean=mk[1],TAXYEAR=mk[2],n_orgs=m['n_orgs'],
               unresolved_grant_orgs=m['unresolved_grant_orgs'],period_unverified_orgs=m['period_unverified_orgs'])
        for side in ('private','public','client'):
            unknown=side=='public' and m['unresolved_grant_orgs']>0
            x['total_'+side]=None if unknown else m[side]
            x['hhi_'+side]=None if unknown or m[side]==0 else m[side+'_sq']/m[side]**2
            x['status_'+side]='unresolved' if unknown else ('active' if m[side]>0 else 'no_observed_revenue')
        x['known_public_revenue_subtotal']=m['public']
        output.append(x)
    write('selected_market_measures.csv',output)
    write('unresolved_organization_keys.csv',affected)
    unresolved=[r for r in output if r['unresolved_grant_orgs']]
    write('unresolved_market_years.csv',unresolved)
    summary=dict(source_rows=nrows,selected_org_years=sum(r['n_orgs'] for r in output),
        market_years=len(output),unresolved_market_years=len(unresolved),
        period_unverified_market_years=sum(r['period_unverified_orgs']>0 for r in output),**counts)
    write('reconstruction_summary.csv',[summary])
    (OUT/'SUCCESS.txt').write_text('Reconstruction complete. Policy-specific sensitivity output, not final certified data.\n')
    print(OUT);print(summary)
if __name__=='__main__':
    try:main()
    except Exception as e:(OUT/'ERROR.txt').write_text(str(e));raise
