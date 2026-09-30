import pandas as pd, numpy as np
ROOT="/sessions/eloquent-pensive-darwin/mnt/NP_Density_1"
RAW=f"{ROOT}/data/raw/census"
OUT=f"{ROOT}/output/revisions/NVSQ_R1_2026-07"
INFL={2012:1.254318,2013:1.236211,2014:1.216477,2015:1.215035,2016:1.199898,
      2017:1.174869,2018:1.146858,2019:1.126447,2020:1.112719,2021:1.062789}
def load(year):
    dp5=pd.read_csv(f"{RAW}/productDownload_2025-09-DP05/ACSDP5Y{year}.DP05-Data.csv",
        skiprows=[1],usecols=["GEO_ID","NAME","DP05_0001E"],low_memory=False)
    dp3=pd.read_csv(f"{RAW}/productDownload_2025-09-DP03/ACSDP5Y{year}.DP03-Data.csv",
        skiprows=[1],usecols=["GEO_ID","DP03_0062E","DP03_0009PE","DP03_0128PE"],low_memory=False)
    d=dp5.merge(dp3,on="GEO_ID")
    d["CBSA"]=d.GEO_ID.str[-5:].astype(int)
    d["TAXYEAR"]=year
    for c,n in [("DP05_0001E","pop_total"),("DP03_0062E","mhi"),
                ("DP03_0009PE","unemployment_rate"),("DP03_0128PE","poverty_rate")]:
        d[n]=pd.to_numeric(d[c],errors="coerce")
    d["metro_micro"]=np.where(d.NAME.str.contains("Micro Area"),"Micro",
                     np.where(d.NAME.str.contains("Metro Area"),"Metro","Other"))
    return d[["CBSA","TAXYEAR","NAME","pop_total","mhi","poverty_rate","unemployment_rate","metro_micro"]]
allyears=pd.concat([load(y) for y in range(2012,2022)],ignore_index=True)
allyears["inflator_2022"]=allyears.TAXYEAR.map(INFL)
allyears["mhi_real_2022"]=allyears.mhi*allyears.inflator_2022
allyears["log_pop"]=np.log(allyears.pop_total)
allyears["log_mhi_real_2022"]=np.log(allyears.mhi_real_2022)
allyears["pop_size_class"]=pd.cut(allyears.pop_total,[0,1e5,2.5e5,1e6,np.inf],
    labels=["<100k","100k-250k","250k-1M","1M+"]).astype(str)
allyears["source_acs"]=allyears.TAXYEAR.map(lambda y:f"acs5_{y}")
allyears.to_csv(f"{OUT}/tables/cbsa_acs5_controls_2012_2021.csv",index=False)
print("rows",len(allyears),"| CBSAs",allyears.CBSA.nunique(),"| years",allyears.TAXYEAR.nunique())
print("metro_micro:",allyears.metro_micro.value_counts().to_dict())
print("missing:",allyears[["pop_total","mhi","poverty_rate","unemployment_rate"]].isna().sum().to_dict())
# coverage + value comparison
m=pd.read_csv(f"{ROOT}/data/processed/market_level_summaries.csv",usecols=["CBSA","TAXYEAR"]).drop_duplicates()
old=pd.read_csv(f"{ROOT}/data/derived/cbsa_acs_controls_wide_2012_2022.csv").rename(columns={"year":"TAXYEAR"})
cmp_rows=[]
for y in range(2012,2022):
    mk=m[m.TAXYEAR==y]
    o=mk.merge(old[old.TAXYEAR==y][["CBSA","pop_total"]],on="CBSA",how="left")
    r=mk.merge(allyears[allyears.TAXYEAR==y][["CBSA","pop_total"]],on="CBSA",how="left")
    cmp_rows.append(dict(TAXYEAR=y,market_cbsas=len(mk),
        old_covered=int(o.pop_total.notna().sum()),old_pct=round(100*o.pop_total.notna().mean(),1),
        rebuilt_covered=int(r.pop_total.notna().sum()),rebuilt_pct=round(100*r.pop_total.notna().mean(),1)))
cov=pd.DataFrame(cmp_rows)
# value agreement on overlap
ov=old.merge(allyears,on=["CBSA","TAXYEAR"],suffixes=("_old","_new"))
vals=[]
for v in ["pop_total","mhi_real_2022","poverty_rate","unemployment_rate"]:
    a,b=ov[v+"_old"],ov[v+"_new"]
    ok=a.notna()&b.notna()
    vals.append(dict(variable=v,N_overlap=int(ok.sum()),corr=round(np.corrcoef(a[ok],b[ok])[0,1],4),
        mean_old=round(a[ok].mean(),1),mean_new=round(b[ok].mean(),1),
        median_abs_pct_diff=round((100*(b[ok]-a[ok]).abs()/a[ok].abs()).median(),2)))
val=pd.DataFrame(vals)
cov.to_csv(f"{OUT}/diagnostics/acs_rebuild_coverage_comparison.csv",index=False)
val.to_csv(f"{OUT}/diagnostics/acs_rebuild_value_comparison.csv",index=False)
print("\n== coverage =="); print(cov.to_string(index=False))
print("\n== value agreement (overlap CBSA-years) =="); print(val.to_string(index=False))
