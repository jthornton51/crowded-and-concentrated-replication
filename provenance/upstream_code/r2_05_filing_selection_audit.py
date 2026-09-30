"""Audit a conservative filing-selection rule; never alter production data."""
import csv
from pathlib import Path
from collections import defaultdict, Counter
from datetime import datetime

ROOT = Path(__file__).resolve().parents[2]
VALID = ROOT / 'output/r2_grant_validation_20260915_144324'
OUT = ROOT / 'output/revisions/NVSQ_R2_2026-09' / ('filing_selection_' + datetime.now().strftime('%Y%m%d_%H%M%S'))
OUT.mkdir(parents=True, exist_ok=False)

def reader(path):
    with path.open(encoding='utf-8-sig', newline='') as f:
        yield from csv.DictReader(f)

def missing(x):
    return x in ('', 'NA', None)

results = []
for year in range(2012, 2022):
    wanted = {r['EIN'] for r in reader(VALID / f'duplicate_keys_{year}.csv') if r['in_saved_panel']=='TRUE'}
    groups = defaultdict(list)
    for r in reader(ROOT / f'data/raw/irs/F9-P08-T00-REVENUE-{year}.csv'):
        ein = r['EIN2'].replace('EIN-', '').replace('-', '').zfill(9)
        if ein in wanted:
            groups[ein].append(r)
    for ein, records in groups.items():
        periods = {(r.get('TAX_PERIOD_BEGIN_DATE'),r.get('TAX_PERIOD_END_DATE')) for r in records}
        grants = {r.get('F9_08_REV_CONTR_GOVT_GRANT') for r in records}
        selected = None
        if len(grants)==1:
            status = 'grant_value_identical_no_filing_choice_needed_for_this_measure'
        elif len(periods)!=1 or any(missing(v) for p in periods for v in p):
            status = 'unresolved_multiple_or_missing_tax_periods'
        elif any(missing(r.get('RETURN_TIME_STAMP')) for r in records):
            status = 'unresolved_missing_timestamp'
        else:
            try:
                stamps = [datetime.fromisoformat(r['RETURN_TIME_STAMP'].replace('Z','+00:00')) for r in records]
                last = max(stamps)
                candidates = [r for r,t in zip(records,stamps) if t==last]
                if len({r.get('F9_08_REV_CONTR_GOVT_GRANT') for r in candidates})!=1:
                    status = 'unresolved_latest_timestamp_tie'
                elif any(r.get('RETURN_AMENDED_X') not in ('TRUE','FALSE') for r in records):
                    status = 'unresolved_amendment_flag'
                elif any(r.get('RETURN_AMENDED_X')=='TRUE' for r in records) and not all(r.get('RETURN_AMENDED_X')=='TRUE' for r in candidates):
                    status = 'unresolved_latest_not_amended_but_earlier_amendment'
                else:
                    selected = candidates[0]
                    status = 'candidate_latest_same_period_amended' if selected['RETURN_AMENDED_X']=='TRUE' else 'candidate_latest_same_period_nonamended'
            except (ValueError, TypeError):
                status = 'unresolved_invalid_or_incomparable_timestamps'
        results.append(dict(EIN=ein,TAXYEAR=year,n_records=len(records),n_periods=len(periods),
            n_grant_values=len(grants),any_amendment=any(r.get('RETURN_AMENDED_X')=='TRUE' for r in records),
            status=status,candidate_objectid=selected.get('OBJECTID','') if selected else '',
            candidate_grants=selected.get('F9_08_REV_CONTR_GOVT_GRANT','') if selected else ''))
    print('Completed',year,flush=True)
with (OUT/'filing_selection_candidates.csv').open('w',newline='',encoding='utf-8') as f:
    w=csv.DictWriter(f,fieldnames=results[0]);w.writeheader();w.writerows(results)
counts=Counter(r['status'] for r in results)
with (OUT/'selection_summary.csv').open('w',newline='',encoding='utf-8') as f:
    w=csv.writer(f);w.writerow(['status','n_keys']);w.writerows(sorted(counts.items()))
(OUT/'SUCCESS.txt').write_text('Metadata audit complete. Candidate choices are not authoritative filing matches.\n')
print(OUT)
print(dict(counts))
