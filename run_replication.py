"""Reproduce the R2 analysis from deposited inputs. Python 3 standard library only."""
from pathlib import Path
import argparse,csv,hashlib,json,math,os,shutil,subprocess,sys,time
ROOT=Path(__file__).resolve().parent
def sha(p):
 h=hashlib.sha256()
 with p.open('rb')as f:
  for b in iter(lambda:f.read(8*1024*1024),b''):h.update(b)
 return h.hexdigest()
def rows(p):
 with p.open(encoding='utf-8-sig',newline='')as f:return list(csv.DictReader(f))
def preflight(rscript):
 for row in rows(ROOT/'FILES.csv'):
  p=ROOT/row['path']
  if not p.is_file()or sha(p)!=row['sha256']:raise RuntimeError('Missing or changed packaged file: '+row['path'])
 cmd=[rscript,'--vanilla','-e','for(p in c("data.table","fixest","diptest","ggplot2")) cat(p, as.character(packageVersion(p)), "\\n")']
 p=subprocess.run(cmd,cwd=ROOT,env={**os.environ,'LC_ALL':'C'},capture_output=True,text=True)
 if p.returncode:raise RuntimeError('Required R packages unavailable:\n'+p.stderr)
 return p.stdout
def equivalent(a,b):
 if a==b:return True
 if a in ('','NA')or b in ('','NA'):return a in ('','NA')and b in ('','NA')
 try:return math.isclose(float(a),float(b),rel_tol=1e-10,abs_tol=1e-10)
 except ValueError:return False
def compare(expected,actual):
 checks=[];failures=[]
 for p in sorted(expected.glob('*.csv')):
  # This is the saved report of a separate historical old/new-run comparison,
  # not a product of the analysis driver. Its evidence is preserved separately.
  if p.name=='reporting_alignment_verification.csv':continue
  q=actual/p.name
  if not q.exists():failures.append({'file':p.name,'error':'missing'});continue
  a,b=rows(p),rows(q)
  if len(a)!=len(b):failures.append({'file':p.name,'error':'row count','expected':len(a),'actual':len(b)});continue
  if not a:checks.append({'file':p.name,'rows':0,'cells':0});continue
  if set(a[0])!=set(b[0]):failures.append({'file':p.name,'error':'columns'});continue
  keys=[k for k in ['scenario','sample','measure','model','term','TAXYEAR','FTYPE','side','size','ntee_broad_clean','CBSA','EIN','composite','active_mix','quantity','min_reporting_pct','reporting_group']if k in a[0]]
  key=lambda r:tuple(r[k]for k in keys)
  if keys and len({key(r)for r in a})==len(a)and len({key(r)for r in b})==len(b):a,b=sorted(a,key=key),sorted(b,key=key)
  bad=[]
  for i,(x,y)in enumerate(zip(a,b)):
   for col in x:
    if not equivalent(x[col],y[col]):bad.append({'row':i,'column':col,'expected':x[col],'actual':y[col]})
  checks.append({'file':p.name,'rows':len(a),'cells':len(a)*len(a[0]),'mismatches':len(bad)})
  if bad:failures.append({'file':p.name,'mismatches':len(bad),'examples':bad[:10]})
 return {'checks':checks,'failures':failures}
def main():
 ap=argparse.ArgumentParser();ap.add_argument('--rscript',default=shutil.which('Rscript'));ap.add_argument('--r-library',type=Path);ap.add_argument('--preflight',action='store_true');args=ap.parse_args()
 if args.r_library:
  os.environ['R_LIBS_USER']=str(args.r_library.resolve(strict=True))
 if not args.rscript:ap.error('Rscript not found; specify --rscript /path/to/Rscript')
 env=preflight(args.rscript);print('Input hashes and R dependencies verified.\n'+env,flush=True)
 if args.preflight:return
 run=ROOT/'verification'/time.strftime('%Y%m%d_%H%M%S');run.mkdir(parents=True,exist_ok=False)
 (run/'package_versions.txt').write_text(env,encoding='utf-8')
 stages=[];locations={};started=time.monotonic();parent=ROOT/'output/revisions/NVSQ_R2_2026-09/rebuild';parent.mkdir(parents=True,exist_ok=True)
 def execute(group,script,arguments,prefix=None,explicit=None):
  before=set(parent.glob(prefix+'*'))if prefix else set()
  cmd=[args.rscript,'--vanilla',str(ROOT/'scripts'/script)]+[str(x)for x in arguments]
  print('Running '+group+' ...',flush=True);t=time.monotonic()
  with (run/(group+'.log')).open('w',encoding='utf-8')as log:
   proc=subprocess.run(cmd,cwd=ROOT,env={**os.environ,'LC_ALL':'C'},stdout=log,stderr=subprocess.STDOUT)
  stage={'stage':group,'exit_code':proc.returncode,'seconds':round(time.monotonic()-t,2)};stages.append(stage)
  (run/'status.json').write_text(json.dumps(stages,indent=2),encoding='utf-8')
  if proc.returncode:raise RuntimeError(group+' failed; see '+str(run/(group+'.log')))
  if explicit:out=explicit
  else:
   found=set(parent.glob(prefix+'*'))-before
   if len(found)!=1:raise RuntimeError('Expected one new output directory for '+group)
   out=found.pop()
  if not (out/'SUCCESS.txt').exists():raise RuntimeError('No success marker: '+str(out))
  locations[group]=out;print(group+' completed in '+str(stage['seconds'])+' seconds.',flush=True)
  return out
 cp=execute('coupling','r2_12_corrected_coupling.R',[ROOT,ROOT/'inputs/filing_policy',ROOT/'inputs/source_coverage'],'coupling_')
 gp=execute('growth','r2_13_corrected_net_growth.R',[ROOT,cp],'net_growth_')
 execute('sensitivity','r2_14_coupling_sensitivities.R',[ROOT,cp],'coupling_sensitivity_')
 execute('tables','r2_15_combined_descriptives_and_tables.R',[ROOT,cp,gp,parent],'combined_tables_')
 ex=run/'exhibits';execute('exhibits','r2_16_submission_exhibits.R',[ROOT,cp,gp,ex],explicit=ex)
 report={g:compare(ROOT/'expected'/g,p)for g,p in locations.items()}
 report['figures']=[{'file':p.name,'byte_identical':sha(p)==sha(ex/p.name)}for p in (ROOT/'expected/exhibits').glob('*.png')]
 report['seconds']=round(time.monotonic()-started,2)
 report['output_directories']={g:str(p.relative_to(ROOT))for g,p in locations.items()}
 report['numeric_failures']=sum(len(report[g]['failures'])for g in locations)
 report['numeric_cells_checked']=sum(sum(c['cells']for c in report[g]['checks'])for g in locations)
 (run/'comparison.json').write_text(json.dumps(report,indent=2),encoding='utf-8')
 if report['numeric_failures']:raise RuntimeError('Output comparison failed; see '+str(run/'comparison.json'))
 (run/'SUCCESS.txt').write_text('All numeric reference comparisons passed. Figure byte comparisons are recorded separately.\n',encoding='utf-8')
 print(json.dumps({k:report[k]for k in ['numeric_cells_checked','numeric_failures','seconds','figures']},indent=2),flush=True)
if __name__=='__main__':main()
