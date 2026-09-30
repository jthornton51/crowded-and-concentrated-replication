"""Read-only tax-period matching for duplicate grant keys; no production selection."""
import csv
import re
from pathlib import Path
from datetime import datetime
from collections import defaultdict, Counter

ROOT = Path(__file__).resolve().parents[2]
PRIOR = ROOT / 'output/revisions/NVSQ_R2_2026-09/filing_selection_20260915_211605/filing_selection_candidates.csv'
OUT = ROOT / 'output/revisions/NVSQ_R2_2026-09' / ('period_match_' + datetime.now().strftime('%Y%m%d_%H%M%S'))
OUT.mkdir(parents=True, exist_ok=False)

def read(path):
    with path.open(encoding='utf-8-sig', newline='') as f:
        yield from csv.DictReader(f)

def ein(x):
    return x.replace('EIN-', '').replace('-', '').zfill(9)

def date_parts(x):
    if x in ('', 'NA', None): return None
    # PZ can contain year-month only. Never invent a day to force exact matches.
    if re.fullmatch(r'\d{6}(\.0)?', x):
        s = x[:6]
        try: datetime.strptime(s, '%Y%m')
        except ValueError: return None
        return s, None
    if re.fullmatch(r'\d{8}', x):
        try: d = datetime.strptime(x, '%Y%m%d')
        except ValueError: return None
        return d.strftime('%Y%m'), d.strftime('%Y-%m-%d')
    try: d = datetime.fromisoformat(x.replace('Z', '+00:00'))
    except ValueError: return None
    return d.strftime('%Y%m'), d.strftime('%Y-%m-%d')

def write(name, data):
    with (OUT/name).open('w', newline='', encoding='utf-8') as f:
        w=csv.DictWriter(f,fieldnames=list(data[0]));w.writeheader();w.writerows(data)

def main():
    prior={(r['EIN'],r['TAXYEAR']):r for r in read(PRIOR)}
    pz=defaultdict(set)
    print('Reading PZ panel tax periods',flush=True)
    for r in read(ROOT/'data/processed/pz_core_with_geo_sector.csv'):
        k=(ein(r['EIN']),r['TAXYEAR'])
        if k in prior: pz[k].add(r.get('TAX_PERIOD_END',''))
    results=[];examples=[]
    for year in range(2012,2022):
        groups=defaultdict(list)
        for r in read(ROOT/f'data/raw/irs/F9-P08-T00-REVENUE-{year}.csv'):
            k=(ein(r['EIN2']),str(year))
            if k in prior: groups[k].append(r)
        for k, records in groups.items():
            dates=pz.get(k,set()); target=None; chosen=None; eligible=[]; granularity=''
            if len(dates)!=1:
                status='unresolved_missing_or_multiple_PZ_periods'
            elif (target:=date_parts(next(iter(dates)))) is None:
                status='unresolved_unparsed_PZ_period'
            else:
                granularity='day' if target[1] else 'month'
                for r in records:
                    end=date_parts(r.get('TAX_PERIOD_END_DATE'))
                    if end and (end[1]==target[1] if target[1] else end[0]==target[0]):
                        eligible.append(r)
                if not eligible:
                    status='unresolved_no_source_period_match'
                elif len({r['F9_08_REV_CONTR_GOVT_GRANT'] for r in eligible})==1:
                    status='period_matched_unambiguous_grant_value'
                    chosen=eligible[0]  # Value only; no claim that this is the exact PZ filing.
                else:
                    periods={(r.get('TAX_PERIOD_BEGIN_DATE'),r.get('TAX_PERIOD_END_DATE')) for r in eligible}
                    if len(periods)!=1 or any(v in ('','NA',None) for p in periods for v in p):
                        status='unresolved_conflicting_begin_or_end_period'
                    else:
                        try:
                            stamps=[datetime.fromisoformat(r['RETURN_TIME_STAMP'].replace('Z','+00:00')) for r in eligible]
                            last=max(stamps); latest=[r for r,t in zip(eligible,stamps) if t==last]
                            if len({r['F9_08_REV_CONTR_GOVT_GRANT'] for r in latest})!=1:
                                status='unresolved_latest_timestamp_tie'
                            elif any(r.get('RETURN_AMENDED_X') not in ('TRUE','FALSE') for r in eligible):
                                status='unresolved_amendment_flag'
                            elif any(r['RETURN_AMENDED_X']=='TRUE' for r in eligible) and not all(r['RETURN_AMENDED_X']=='TRUE' for r in latest):
                                status='unresolved_amendment_sequence'
                            else:
                                chosen=latest[0]
                                status='period_matched_latest_amended_candidate' if chosen['RETURN_AMENDED_X']=='TRUE' else 'period_matched_latest_nonamended_candidate'
                        except (ValueError,TypeError,KeyError):
                            status='unresolved_timestamp'
            results.append(dict(EIN=k[0],TAXYEAR=year,prior_status=prior[k]['status'],
                prior_conflicting=int(prior[k]['n_grant_values'])>1,
                pz_period_values=' | '.join(sorted(dates)),match_granularity=granularity,
                n_source_records=len(records),n_period_matches=len(eligible),status=status,
                grant_value=chosen['F9_08_REV_CONTR_GOVT_GRANT'] if chosen else '',
                candidate_objectid=chosen.get('OBJECTID','') if chosen else '',
                same_as_prior_candidate=(chosen.get('OBJECTID','')==prior[k]['candidate_objectid']) if chosen and prior[k]['candidate_objectid'] else 'not_applicable'))
            if status.startswith('unresolved'):
                for r in records:
                    examples.append(dict(EIN=k[0],TAXYEAR=year,pz_period_values=' | '.join(sorted(dates)),status=status,
                        **{v:r.get(v,'') for v in ['OBJECTID','RETURN_TYPE','RETURN_AMENDED_X','RETURN_TIME_STAMP','TAX_YEAR','TAX_PERIOD_BEGIN_DATE','TAX_PERIOD_END_DATE','F9_08_REV_CONTR_GOVT_GRANT','URL']}))
        print('Completed',year,flush=True)
    assert len(results)==len(prior), (len(results),len(prior))
    write('period_match_candidates.csv',results)
    if examples: write('unresolved_source_records.csv',examples)
    counts=Counter((r['prior_conflicting'],r['status']) for r in results)
    write('period_match_summary.csv',[dict(prior_conflicting=k[0],status=k[1],n_keys=n) for k,n in sorted(counts.items())])
    (OUT/'SUCCESS.txt').write_text('Period matching complete. Period agreement does not prove exact filing agreement. No source data changed.\n')
    print(OUT);print(dict(counts))

if __name__=='__main__':
    try: main()
    except Exception as e:
        (OUT/'ERROR.txt').write_text(str(e));raise
