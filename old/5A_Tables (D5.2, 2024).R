################################################################################

#CREATE TABLES FOR 2-AREA MODELS

#Upload libraries
library(knitr)
library(kableExtra)

#Choose a period (note: 75 = shock starting period)
yr=75

################################################################################
################################################################################
################################################################################

#Create BS matrix

#Create row names for BS matrix
rownames <-c( "Money",
              "Advances",
              "Deposits",
              "Loans",
              "Area 1 bills",
              "Area 2 bills",
              "Area 1 shares",
              "Area 2 shares",
              "Capital stock",
              "Net financial wealth",
              "Total")

################################################################################

#Create entries for Z1 BS

#Create household aggregates
Z1_Households <- c(round(Z1_h_h[yr], digits = 2),
                0,
                round(Z1_mh[yr], digits = 2),
                round(-Z1_lh[yr], digits = 2),
                round(Z1_b_h_Z1[yr], digits = 2),
                round(Z1_b_h_Z2[yr], digits = 2),
                round(Z1_e_h_Z1[yr], digits = 2),
                round(Z1_e_h_Z2[yr], digits = 2),
                0,
                round(-Z1_v[yr], digits = 2),
                round(Z1_h_h[yr]+Z1_mh[yr]+Z1_b_h_Z1[yr]+Z1_b_h_Z2[yr]-Z1_v[yr]+
                                          +Z1_e_h_Z1[yr]+Z1_e_h_Z2[yr]-Z1_lh[yr], digits = 0)
)                                                                    

#Create table of results
Z1_HouseDataBS<-as.data.frame(Z1_Households,row.names=rownames)
kable(Z1_HouseDataBS) #Unload kableExtra to use this

#Create firms aggregates
Z1_Firms <- c(0,                                                                    
           0,
           0,
           round(-Z1_lf[yr], digits = 2),
           0,
           0,
           round(-Z1_e_s[yr], digits = 2),
           0,
           round(Z1_k[yr]*Z1_pid[yr]-0.01, digits = 2),
           0,
           round(-Z1_lf[yr]+Z1_k[yr]*Z1_pid[yr]-Z1_e_s[yr]-0.01, digits = 2)
)                                                                    

#Create table of results
Z1_FirmsDataBS<-as.data.frame(Z1_Firms,row.names=rownames)
kable(Z1_FirmsDataBS) #Unload kableExtra to use this

#Create government aggregates
Z1_Government <- c(0,
                0,
                0,
                0,
                round(-Z1_b_s[yr], digits = 2),
                0,
                0,
                0,
                0,
                round(Z1_b_s[yr], digits = 2),
                0
)                                                                    

#Create table of results
Z1_GovDataBS<-as.data.frame(Z1_Government,row.names=rownames)
kable(Z1_GovDataBS) #Unload kableExtra to use this

#Create banks aggregates
Z1_Banks       <- c(0,
                 round(-Z1_a_d[yr], digits = 2),
                 round(-Z1_mh[yr], digits = 2),
                 round(Z1_ls[yr], digits = 2),
                 round(Z1_b_b[yr], digits = 2),
                 0,
                 0,
                 0,
                 0,
                 0,
                 round(-Z1_a_d[yr]-Z1_mh[yr]+Z1_ls[yr]+Z1_b_b[yr], digits = 2)
)                                                                    

#Create table of results
Z1_BanksDataBS<-as.data.frame(Z1_Banks,row.names=rownames)
kable(Z1_BanksDataBS) #Unload kableExtra to use this

#Create CB aggregates
Z1_CentralBank <- c(round(-Z1_h_s[yr], digits = 2),
                 round(Z1_a_s[yr], digits = 2),
                 0,
                 0,
                 round(Z1_b_cb[yr], digits = 2),   # <-------------------------- check
                 round(Z1_b_cb_d_Z2[yr], digits = 2),
                 0,
                 0,
                 0,
                 0,
                 round(-Z1_h_s[yr]+Z1_a_s[yr]+Z1_b_cb[yr]+Z1_b_cb_d_Z2[yr], digits = 2)
)                                                                    

#Create table of results
Z1_CBDataBS<-as.data.frame(Z1_CentralBank,row.names=rownames)
kable(Z1_CBDataBS) #Unload kableExtra to use this

################################################################################

#Create entries for Z2 BS

#Create household aggregates
Z2_Households <- c(round(Z2_xr[yr]*Z2_h_h[yr], digits = 2),
                   0,
                   round(Z2_xr[yr]*Z2_mh[yr], digits = 2),
                   round(-Z2_xr[yr]*Z2_lh[yr], digits = 2),
                   round(Z2_xr[yr]*Z2_b_h_Z1[yr], digits = 2),
                   round(Z2_xr[yr]*Z2_b_h_Z2[yr], digits = 2),
                   round(Z2_xr[yr]*Z2_e_h_Z1[yr], digits = 2),
                   round(Z2_xr[yr]*Z2_e_h_Z2[yr], digits = 2),
                   0,
                   round(Z2_xr[yr]*(-Z2_v[yr]), digits = 2),
                   round(Z2_xr[yr]*(Z2_h_h[yr]+Z2_mh[yr]+Z2_b_h_Z1[yr]+Z2_b_h_Z2[yr]-Z2_v[yr]+
                                                        +Z2_e_h_Z1[yr]+Z2_e_h_Z2[yr]-Z2_lh[yr]), digits = 0)
                   
)                                                                    

#Create table of results
Z2_HouseDataBS<-as.data.frame(Z2_Households,row.names=rownames)
kable(Z2_HouseDataBS) #Unload kableExtra to use this

#Create firms aggregates
Z2_Firms <- c(0,                                                                    
              0,
              0,
              round(Z2_xr[yr]*(-Z2_lf[yr]), digits = 2),
              0,
              0,
              0,
              round(Z2_xr[yr]*(-Z2_e_s[yr]), digits = 2),
              round(Z2_xr[yr]*(Z2_k[yr]*Z2_pid[yr]), digits = 2),
              0,
              round(Z2_xr[yr]*(-Z2_lf[yr]+Z2_k[yr]*Z2_pid[yr]-Z2_e_s[yr]), digits = 2)
)                                                                    

#Create table of results
Z2_FirmsDataBS<-as.data.frame(Z2_Firms,row.names=rownames)
kable(Z2_FirmsDataBS) #Unload kableExtra to use this

#Create government aggregates
Z2_Government <- c(0,
                   0,
                   0,
                   0,
                   0,
                   round(Z2_xr[yr]*(-Z2_b_s[yr]), digits = 2),
                   0,
                   0,
                   0,
                   round(Z2_xr[yr]*(Z2_b_s[yr]), digits = 2),
                   0
)                                                                    

#Create table of results
Z2_GovDataBS<-as.data.frame(Z2_Government,row.names=rownames)
kable(Z2_GovDataBS) #Unload kableExtra to use this

#Create banks aggregates
Z2_Banks       <- c(0,
                    round(Z2_xr[yr]*(-Z2_a_d[yr]), digits = 2),
                    round(Z2_xr[yr]*(-Z2_mh[yr]), digits = 2),
                    round(Z2_xr[yr]*Z2_ls[yr], digits = 2),
                    0,
                    round(Z2_xr[yr]*Z2_b_b[yr], digits = 2),
                    0,
                    0,
                    0,
                    0,
                    round(Z2_xr[yr]*(-Z2_a_d[yr]-Z2_mh[yr]+Z2_ls[yr]+Z2_b_b[yr]), digits = 2)
)                                                                    

#Create table of results
Z2_BanksDataBS<-as.data.frame(Z2_Banks,row.names=rownames)
kable(Z2_BanksDataBS) #Unload kableExtra to use this

#Create CB aggregates
Z2_CentralBank <- c(round(Z2_xr[yr]*(-Z2_h_s[yr]), digits = 2),
                    round(Z2_xr[yr]*(Z2_a_s[yr]), digits = 2),
                    0,
                    0,
                    0,
                    round(Z2_xr[yr]*(Z2_b_cb[yr]), digits = 2), 
                    0,
                    0,
                    0,
                    0,
                    round(Z2_xr[yr]*(-Z2_h_s[yr]+Z2_a_s[yr]+Z2_b_cb[yr]), digits = 2)
)                                                                    

#Create table of results
Z2_CBDataBS<-as.data.frame(Z2_CentralBank,row.names=rownames)
kable(Z2_CBDataBS) #Unload kableExtra to use this

################################################################################

#Create total aggregates
Total <- c( 0,
            0,
            0,
            round(Z1_ls[yr]-Z1_lh[yr]-Z1_lf[yr]+Z2_xr[yr]*(Z2_ls[yr]-Z2_lh[yr]-Z2_lf[yr]), digits=2),
            round(Z1_b_h_Z1[yr]-Z1_b_s[yr]+Z1_b_b[yr]+Z1_b_cb[yr]+Z2_xr[yr]*Z2_b_h_Z1[yr], digits=2),
            round(Z2_b_h_Z2[yr]-Z2_b_s[yr]+Z2_b_b[yr]+Z2_b_cb[yr]+Z1_xr[yr]*Z1_b_h_Z2[yr] + Z1_b_cb_s_Z2[yr], digits=2),
            round(Z1_e_h_Z1[yr]-Z1_e_s[yr]+Z2_xr[yr]*Z2_e_h_Z1[yr], digits=2),
            round(Z2_e_h_Z2[yr]-Z2_e_s[yr]+Z1_xr[yr]*Z1_e_h_Z2[yr], digits=2),
            
            round(Z1_k[yr]*Z1_pid[yr] + Z2_xr[yr]*(Z2_k[yr]*Z2_pid[yr]-0.01), digits=2),
            round(-Z1_v[yr] + Z1_b_s[yr] - Z2_xr[yr]*(Z2_v[yr] - Z2_b_s[yr]), digits = 2),
            
            round(Z1_ls[yr]-Z1_lh[yr]-Z1_lf[yr]+Z2_xr[yr]*(Z2_ls[yr]-Z2_lh[yr]-Z2_lf[yr]) +
                  Z1_b_h_Z1[yr]-Z1_b_s[yr]+Z1_b_b[yr]+Z1_b_cb[yr] + Z2_xr[yr]*Z2_b_h_Z1[yr]+
                  Z2_b_h_Z2[yr]-Z2_b_s[yr]+Z2_b_b[yr]+Z2_b_cb[yr] + Z1_xr[yr]*Z1_b_h_Z2[yr] + Z1_b_cb_s_Z2[yr]+
                  Z1_k[yr]*Z1_pid[yr] + Z2_xr[yr]*(Z2_k[yr]*Z2_pid[yr])+
                  -Z1_v[yr] + Z1_b_s[yr] - Z2_xr[yr]*(Z2_v[yr] - Z2_b_s[yr]) +
                  + Z1_e_h_Z1[yr]-Z1_e_s[yr]+Z2_xr[yr]*Z2_e_h_Z1[yr] + Z2_e_h_Z2[yr]-Z2_e_s[yr]+Z1_xr[yr]*Z1_e_h_Z2[yr]-0.01, digits = 2)
                  
)                                                                    

#Create table of results
TotDataBS<-as.data.frame(Total,row.names=rownames)
kable(TotDataBS) #Unload kableExtra to use this

#Create xr column
xr <- c( round(Z1_xr[yr], digits=4),
            round(Z1_xr[yr], digits=4),
            round(Z1_xr[yr], digits=4),
            round(Z1_xr[yr], digits=4),
            round(Z1_xr[yr], digits=4),
            round(Z1_xr[yr], digits=4),
            round(Z1_xr[yr], digits=4),
            round(Z1_xr[yr], digits=4),
            round(Z1_xr[yr], digits=4),
            round(Z1_xr[yr], digits=4),
            paste("")
)        

#Create BS matrix
BS_Matrix<-cbind(Z1_HouseDataBS,Z1_FirmsDataBS,Z1_GovDataBS,Z1_BanksDataBS,Z1_CBDataBS,xr,
                 Z2_HouseDataBS,Z2_FirmsDataBS,Z2_GovDataBS,Z2_BanksDataBS,Z2_CBDataBS,TotDataBS)
kable(BS_Matrix) #Unload kableExtra to use this

################################################################################
################################################################################

#Create html version of tables

#Create titles
if (shock==1){title <- paste("increase in propensity to import of Area 2")}
if (shock==2){title <- paste("devaluation of Area 1 currency")}
if (shock==3){title <- paste("increase in propensity to import of Area 2 and quasi-floating exchange rate")}
if (shock==4){title <- paste("CE innovation in Area 1 with fixed exchange rate")}
if (shock==5){title <- paste("CE innovation in Area 1 with floating exchange rate")}
caption1 <- paste("Table 3. Balance sheet (OPEN) in period", yr) #"after",title
caption2 <- paste("Table 2. Transactions-flow matrix (OPEN) in period ",yr) #,"after",title


#Create html table for BS
BS_Matrix %>%
  kbl(caption=caption1,
      format= "html",
      #format= "latex",
      col.names = c("H1","F1","G1","B1","CB1","xr1","H2","F2","G2","B2","CB2","Tot"),
      align="r") %>%
  kable_classic(full_width = F, html_font = "helvetica")



################################################################################

#Create TFM matrix

#Create row names for TFM matrix
rownames <-c( "Consumption",
              "Investment",
              "Government spending",
              "Export of Area 1",
              "Import of Area 1",
              "[Value added]",
              "Wage bill",
              "Corporate profit",
              "Amortization",
              "Bank profit",
              "CB profit",
              "Income tax revenue",
              "VAT revenue",
              "Tariffs revenue",
              "Interests on deposits",
              "Interests on loans",
              "Interests on Area 1 bills",
              "Interests on Area 2 bills",
              "Change in money stock",
              "Change in advances",
              "Change in deposits",
              "Change in loans",
              "Change in Area 1 bills",
              "Change in Area 2 bills",
              "Change in Area 1 shares",
              "Change in Area 2 shares",
              "Revaluation effects",
              "Total"
)


################################################################################

#Create entries for Z1 TFM

#Create household aggregates
Z1_Households <- c(round(-Z1_c[yr]*Z1_pa[yr], digits = 2),                                                                    
                0,
                0,
                0,
                0,
                0,
                round(Z1_wb[yr], digits = 2),
                round(Z1_div[yr], digits = 2),
                0,
                round(Z1_f_b[yr], digits = 2),
                0,
                round(-Z1_t[yr], digits = 2),
                0,
                0,
                round(Z1_rm[yr-1]*Z1_mh[yr-1], digits = 2),
                round(-Z1_rh[yr-1]*Z1_lh[yr-1], digits = 2),
                round(Z1_rb[yr-1] * Z1_b_s_Z1[yr-1], digits = 2),
                round(Z2_rb[yr-1] * Z1_b_h_Z2[yr-1], digits = 2),
                round(-Z1_h_h[yr]+Z1_h_h[yr-1], digits = 2),
                0,
                round(-Z1_mh[yr]+Z1_mh[yr-1], digits = 2),
                round(Z1_lh[yr]-Z1_lh[yr-1], digits = 2),
                round(-Z1_b_h_Z1[yr]+Z1_b_h_Z1[yr-1], digits = 2),
                round(-Z1_b_h_Z2[yr]+Z1_b_h_Z2[yr-1], digits = 2),
                
                round(-Z1_e_h_Z1[yr]+Z1_e_h_Z1[yr-1], digits = 2),
                round(-Z1_e_h_Z2[yr]+Z1_e_h_Z2[yr-1], digits = 2),
                
                round(Z2_xr[yr]*(Z1_b_s_Z2[yr]+Z1_e_s_Z2[yr])*(Z2_xr[yr]-Z2_xr[yr-1]), digits = 2),
                
                round(-Z1_c[yr]*Z1_pa[yr]+Z1_wb[yr]+Z1_div[yr]+Z1_f_b[yr]-Z1_t[yr]+
                      +Z1_rm[yr-1]*Z1_mh[yr-1]+
                      -Z1_rh[yr-1]*Z1_lh[yr-1]
                      +Z1_rb[yr-1]*Z1_b_s_Z1[yr-1]+
                      +Z2_rb[yr-1]*Z1_b_h_Z2[yr-1]+
                      -Z1_h_h[yr]+Z1_h_h[yr-1]+
                      -Z1_mh[yr]+Z1_mh[yr-1]+
                      +Z1_lh[yr]-Z1_lh[yr-1]
                      -Z1_b_h_Z1[yr]+Z1_b_h_Z1[yr-1]+
                      -Z1_b_h_Z2[yr]+Z1_b_h_Z2[yr-1]+
                      -Z1_e_h_Z1[yr]+Z1_e_h_Z1[yr-1]+
                      -Z1_e_h_Z2[yr]+Z1_e_h_Z2[yr-1]+
                      +Z2_xr[yr]*(Z1_b_s_Z2[yr]+Z1_e_s_Z2[yr])*(Z2_xr[yr]-Z2_xr[yr-1])
                      , digits = 2)
)

#Create table of results
Z1_HouseDataTFM<-as.data.frame(Z1_Households,row.names=rownames)
kable(Z1_HouseDataTFM)

#Create firms aggregates (current account)
Z1_Firms <- c(round(Z1_c[yr]*Z1_pa[yr], digits = 2),                                                                    
           round(Z1_id[yr]*Z1_pid[yr]+Z1_id_g[yr]*Z1_pid[yr], digits = 2),
           round(Z1_g[yr]*Z1_pg[yr], digits = 2),
           round(Z1_nex[,yr], digits = 2),
           round(-Z1_nimp[,yr], digits = 2),
           paste("[",round(Z1_yn[yr], digits = 2),"]"),
           round(-Z1_wb[yr], digits = 2),
           round(-Z1_f_f[yr], digits = 2),
           round(-Z1_af[yr], digits = 2),
           0,
           0,
           0,
           round(-Z1_vat_rev[yr], digits = 2),
           round(-Z2_xr[yr]*Z2_tar_rev[yr], digits = 2),
           0,
           round(-Z1_rl[yr-1]*Z1_lf[yr-1], digits = 2),
           0,
           0,
           0,
           0,
           0,
           0,
           0,
           0,
           0,
           0,
           0,
           round(Z1_c[yr]*Z1_pa[yr] + Z1_id[yr]*Z1_pid[yr]+ Z1_id_g[yr]*Z1_pid[yr] + Z1_g[yr]*Z1_pg[yr] + Z1_nex[,yr] - Z1_nimp[,yr] +
                 -Z1_wb[yr] - Z1_f_f[yr] - Z1_af[yr] - Z1_rl[yr-1]*Z1_lf[yr-1] - Z1_vat_rev[yr]  - Z2_xr[yr]*Z2_tar_rev[yr], digits = 2)
)

#Create table of results
Z1_FirmsDataTFM<-as.data.frame(Z1_Firms,row.names=rownames)
kable(Z1_FirmsDataTFM)

#Create capital aggregates
Z1_Capital <- c(0,                                                                    
             round(-Z1_id[yr]*Z1_pid[yr], digits = 2),
             0,
             0,
             0,
             0,
             0,
             0,
             round(Z1_af[yr], digits = 2),
             round(Z1_f_f_u[yr], digits = 2),
             0,
             0,
             0,
             0,
             0,
             0,
             0,
             0,
             0,
             0,
             0,
             round(Z1_lf[yr]-Z1_lf[yr-1], digits = 2),
             0,
             0,
             round(Z1_e_s[yr]-Z1_e_s[yr-1], digits = 2),
             0,
             0,
             round(-Z1_id[yr]*Z1_pid[yr] + Z1_af[yr] + (Z1_lf[yr]-Z1_lf[yr-1]) +
                   +Z1_f_f_u[yr]+Z1_e_s[yr]-Z1_e_s[yr-1], digits = 2)
)

#Create table of results
Z1_CapitalDataTFM<-as.data.frame(Z1_Capital,row.names=rownames)
kable(Z1_CapitalDataTFM)

#Create government aggregates
Z1_Government <- c( 0,
                 round(-Z1_id_g[yr]*Z1_pid[yr], digits = 2),
                 round(-Z1_g[yr]*Z1_pg[yr], digits = 2),
                 0,
                 0,
                 0,
                 0,
                 0,
                 0,
                 0,
                 round(Z1_f_cb[yr], digits = 2),
                 round(Z1_t[yr], digits = 2),
                 round(Z1_vat_rev[yr], digits = 2),
                 round(Z1_tar_rev[yr], digits = 2),
                 0,
                 0,
                 round(-Z1_rb[yr-1]*Z1_b_s[yr-1], digits = 2), 
                 0,
                 0,
                 0,
                 0,
                 0,
                 round(Z1_b_s[yr]-Z1_b_s[yr-1] - Z2_b_s_Z1[yr]*(Z1_xr[yr]-Z1_xr[yr-1]) , digits = 2),
                 0,
                 
                 0, #round(-Z2_b_s_Z1[yr]*(Z1_xr[yr]-Z1_xr[yr-1]) , digits = 2),
                 0,
                 0,
                 round(-Z1_g[yr]*Z1_pg[yr]-Z1_id_g[yr]*Z1_pid[yr]+Z1_f_cb[yr]+Z1_t[yr]-Z1_rb[yr-1]*Z1_b_s[yr-1]+(Z1_b_s[yr]-Z1_b_s[yr-1]) + Z1_vat_rev[yr] + Z1_tar_rev[yr]
                       #-Z2_b_s_Z1[yr]*(Z1_xr[yr]-Z1_xr[yr-1])
                       , digits = 2)
)

#Create table of results
Z1_GovDataTFM<-as.data.frame(Z1_Government,row.names=rownames)
kable(Z1_GovDataTFM)

#Create central bank aggregates
Z1_CentralBank <- c( 0,                                                                    
                  0,
                  0,
                  0,
                  0,
                  0,
                  0,
                  0,
                  0,
                  0,
                  round(-Z1_f_cb[yr], digits = 2),
                  0,
                  0,
                  0,
                  0,
                  0,
                  round(Z1_rb[yr-1] * Z1_b_cb[yr-1], digits = 2),
                  round(Z2_rb[yr-1] * Z1_b_cb_s_Z2[yr-1] * Z2_xr[yr], digits = 2),
                  round(Z1_h_s[yr]-Z1_h_s[yr-1], digits = 2),
                  round(-Z1_a_s[yr]+Z1_a_s[yr-1], digits = 2),
                  0,
                  0,
                  round(-Z1_b_cb[yr]+Z1_b_cb[yr-1], digits = 2),
                  round(-Z1_b_cb_d_Z2[yr]+Z1_b_cb_d_Z2[yr-1], digits = 2),
                  
                  0, #round(Z2_xr[yr]*Z1_b_cb_s_Z2[yr]*(Z2_xr[yr]-Z2_xr[yr-1]), digits = 2),
                  0,
                  0,
                  
                  round(-Z1_f_cb[yr]+Z1_rb[yr-1] * Z1_b_cb[yr-1]+Z2_rb[yr-1] * Z1_b_cb_s_Z2[yr-1] * Z2_xr[yr]+
                        +(Z1_h_s[yr]-Z1_h_s[yr-1])
                        -Z1_a_s[yr]+Z1_a_s[yr-1]
                        -(Z1_b_cb[yr]-Z1_b_cb[yr-1])
                        -(Z1_b_cb_d_Z2[yr]-Z1_b_cb_d_Z2[yr-1])
                        #+Z2_xr[yr]*Z1_b_cb_s_Z2[yr]*(Z2_xr[yr]-Z2_xr[yr-1])
                        , digits = 2)
)

#Create table of results
Z1_CBDataTFM<-as.data.frame(Z1_CentralBank,row.names=rownames)
kable(Z1_CBDataTFM)

#Create banks aggregates
Z1_Banks       <- c( 0,                                                                    
                  0,
                  0,
                  0,
                  0,
                  0,
                  0,
                  0,
                  0,
                  round(-Z1_f_b[yr], digits = 2),
                  0,
                  0,
                  0,
                  0,
                  round(-Z1_rm[yr-1]*Z1_mh[yr-1], digits = 2),
                  round(Z1_rl[yr-1]*Z1_lf[yr-1]+Z1_rh[yr-1]*Z1_lh[yr-1], digits = 2),
                  round(Z1_rb[yr-1]*Z1_b_b[yr-1], digits = 2),
                  0,
                  0,
                  round(Z1_a_d[yr]-Z1_a_d[yr-1], digits = 2),
                  round(Z1_mh[yr]-Z1_mh[yr-1], digits = 2),
                  round(-Z1_ls[yr]+Z1_ls[yr-1] , digits = 2),
                  round(-Z1_b_b[yr]+Z1_b_b[yr-1], digits = 2),
                  0,
                  0,
                  0,
                  0,
                  round(-Z1_f_b[yr] - Z1_rm[yr-1]*Z1_mh[yr-1] + Z1_rl[yr-1]*Z1_lf[yr-1] + Z1_rh[yr-1]*Z1_lh[yr-1] + Z1_rb[yr-1]*Z1_b_b[yr-1] + (Z1_a_d[yr]-Z1_a_d[yr-1])
                          - (-Z1_mh[yr]+Z1_mh[yr-1] + Z1_ls[yr]-Z1_ls[yr-1]  + Z1_b_b[yr]-Z1_b_b[yr-1] )  , digits = 2)
)

#Create table of results
Z1_BanksDataTFM<-as.data.frame(Z1_Banks,row.names=rownames)
kable(Z1_BanksDataTFM)

################################################################################

#Create entries for Z2 TFM

#Create household aggregates
Z2_Households <- c(round(Z2_xr[yr]*(-Z2_c[yr]*Z2_pa[yr]), digits = 2),                                                                    
                   0,
                   0,
                   0,
                   0,
                   0,
                   round(Z2_xr[yr]*(Z2_wb[yr]), digits = 2),
                   round(Z2_xr[yr]*(Z2_div[yr]), digits = 2),
                   0,
                   round(Z2_xr[yr]*(Z2_f_b[yr]), digits = 2),
                   0,
                   round(Z2_xr[yr]*(-Z2_t[yr]), digits = 2),
                   0,
                   0,
                   round(Z2_xr[yr]*(Z2_rm[yr-1]*Z2_mh[yr-1]), digits = 2),
                   round(-Z2_xr[yr]*(Z2_rh[yr-1]*Z2_lh[yr-1]), digits = 2),
                   round(Z1_rb[yr-1] * Z2_b_s_Z1[yr-1], digits = 2),
                   round(Z2_xr[yr]*Z2_rb[yr-1] * Z2_b_h_Z2[yr-1], digits = 2),
                   round(Z2_xr[yr]*(-Z2_h_h[yr]+Z2_h_h[yr-1]), digits = 2),
                   0,
                   round(Z2_xr[yr]*(-Z2_mh[yr]+Z2_mh[yr-1]), digits = 2),
                   round(Z2_xr[yr]*(Z2_lh[yr]-Z2_lh[yr-1]), digits = 2),
                   round(-Z2_b_s_Z1[yr]+Z2_b_s_Z1[yr-1], digits = 2),
                   
                   round(Z2_xr[yr]*(-Z2_b_h_Z2[yr]+Z2_b_h_Z2[yr-1]), digits = 2),
                   
                   round(-Z2_e_s_Z1[yr]+Z2_e_s_Z1[yr-1], digits = 2),
                   
                   round(Z2_xr[yr]*(-Z2_e_h_Z2[yr]+Z2_e_h_Z2[yr-1]), digits = 2),
                   
                   round((Z2_b_s_Z1[yr]+Z2_e_s_Z1[yr])*(Z1_xr[yr]-Z1_xr[yr-1]), digits = 2),
                   
                   round(Z2_xr[yr]*(-Z2_c[yr]*Z2_pa[yr]+Z2_wb[yr]+Z2_div[yr]+Z2_f_b[yr]-Z2_t[yr]+
                         +Z2_rm[yr-1]*Z2_mh[yr-1]+
                         -Z2_rh[yr-1]*Z2_lh[yr-1]+
                         +Z1_rb[yr-1] * Z2_b_h_Z1[yr-1]+
                         +Z2_rb[yr-1] * Z2_b_h_Z2[yr-1]+
                         -Z2_h_h[yr]+Z2_h_h[yr-1]+
                         -Z2_mh[yr]+Z2_mh[yr-1]+
                         +Z2_lh[yr]-Z2_lh[yr-1]+
                         -Z2_b_h_Z1[yr]+Z2_b_h_Z1[yr-1]+
                         -Z2_b_h_Z2[yr]+Z2_b_h_Z2[yr-1]+
                         -Z2_e_h_Z1[yr]+Z2_e_h_Z1[yr-1]+
                         -Z2_e_h_Z2[yr]+Z2_e_h_Z2[yr-1])+   
                         +(Z2_b_s_Z1[yr]+Z2_e_s_Z1[yr])*(Z1_xr[yr]-Z1_xr[yr-1]), digits = 2)
)

#Create table of results
Z2_HouseDataTFM<-as.data.frame(Z2_Households,row.names=rownames)
kable(Z2_HouseDataTFM)

#Create firms aggregates (current account)
Z2_Firms <- c(round(Z2_xr[yr]*Z2_c[yr]*Z2_pa[yr], digits = 2),                                                                    
              round(Z2_xr[yr]*Z2_id[yr]*Z2_pid[yr]+Z2_xr[yr]*Z2_id_g[yr]*Z2_pid[yr], digits = 2),
              round(Z2_xr[yr]*Z2_g[yr]*Z2_pg[yr], digits = 2),
              round(Z2_xr[yr]*(-Z2_nimp[,yr]), digits = 2),
              round(Z2_xr[yr]*Z2_nex[,yr], digits = 2),
              paste("[",round(Z2_xr[yr]*Z2_yn[yr], digits = 2),"]"),
              round(-Z2_xr[yr]*Z2_wb[yr], digits = 2),
              round(-Z2_xr[yr]*Z2_f_f[yr], digits = 2),
              round(-Z2_xr[yr]*Z2_af[yr], digits = 2),
              0,
              0,
              0,
              round(-Z2_xr[yr]*Z2_vat_rev[yr], digits = 2),
              round(-Z1_tar_rev[yr], digits = 2),               #Note: already expressed in Z1 currency
              0,
              round(-Z2_xr[yr]*Z2_rl[yr-1]*Z2_lf[yr-1], digits = 2),
              0,
              0,
              0,
              0,
              0,
              0,
              0,
              0,
              0,
              0,
              0,
              round(Z2_xr[yr]*(Z2_c[yr]*Z2_pa[yr] + Z2_id[yr]*Z2_pid[yr]+Z2_xr[yr]*Z2_id_g[yr]*Z2_pid[yr] + Z2_g[yr]*Z2_pg[yr] + Z2_nex[,yr] - Z2_nimp[,yr] +
                    - Z2_wb[yr] -  Z2_f_f[yr] - Z2_af[yr] - Z2_rl[yr-1]*Z2_lf[yr-1] - Z2_vat_rev[yr] ) - Z1_tar_rev[yr], digits = 2)
)

#Create table of results
Z2_FirmsDataTFM<-as.data.frame(Z2_Firms,row.names=rownames)
kable(Z2_FirmsDataTFM)

#Create capital aggregates
Z2_Capital <- c(0,                                                                    
                round(-Z2_xr[yr]*Z2_id[yr]*Z2_pid[yr], digits = 2),
                0,
                0,
                0,
                0,
                0,
                0,
                round(Z2_xr[yr]*Z2_af[yr], digits = 2),
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                round(Z2_xr[yr]*(Z2_lf[yr]-Z2_lf[yr-1]), digits = 2),
                0,
                0,
                0,
                round(Z2_xr[yr]*(Z2_e_s[yr]-Z2_e_s[yr-1]), digits = 2),
                0,
                round(Z2_xr[yr]*(-Z2_id[yr]*Z2_pid[yr] + Z2_af[yr] + (Z2_lf[yr]-Z2_lf[yr-1])) +
                     + Z2_xr[yr]*(Z2_e_s[yr]-Z2_e_s[yr-1]), digits = 2)
)

#Create table of results
Z2_CapitalDataTFM<-as.data.frame(Z2_Capital,row.names=rownames)
kable(Z2_CapitalDataTFM)

#Create government aggregates
Z2_Government <- c( 0,
                    round(-Z2_xr[yr]*Z2_id_g[yr]*Z2_pid[yr], digits = 2),
                    round(-Z2_xr[yr]*Z2_g[yr]*Z2_pg[yr], digits = 2),
                    0,
                    0,
                    0,
                    0,
                    0,
                    0,
                    0,
                    round(Z2_xr[yr]*Z2_f_cb[yr], digits = 2),
                    round(Z2_xr[yr]*Z2_t[yr], digits = 2),
                    round(Z2_xr[yr]*Z2_vat_rev[yr], digits = 2),
                    round(Z2_xr[yr]*Z2_tar_rev[yr], digits = 2),
                    0,
                    0,
                    0,
                    round(-Z2_xr[yr]*Z2_rb[yr-1]*Z2_b_s[yr-1], digits = 2), 
                    0,
                    0,
                    0,
                    0,
                    0,
                    round(Z2_xr[yr]*(Z2_b_s[yr]-Z2_b_s[yr-1]) - Z1_b_s_Z2[yr]*(Z2_xr[yr]-Z2_xr[yr-1]) - Z1_b_cb_s_Z2[yr]*(Z2_xr[yr]-Z2_xr[yr-1]) , digits = 2),
                    
                    0, #round(-(Z1_b_s_Z2[yr]*(Z2_xr[yr]-Z2_xr[yr-1]) + Z1_b_cb_s_Z2[yr]*(Z2_xr[yr]-Z2_xr[yr-1])) , digits = 2),
                    0,
                    0,
                    
                    round(Z2_xr[yr]*(-Z2_g[yr]*Z2_pg[yr]-Z2_id_g[yr]*Z2_pid[yr]+Z2_f_cb[yr]+Z2_t[yr]-Z2_rb[yr-1]*Z2_b_s[yr-1]+(Z2_b_s[yr]-Z2_b_s[yr-1]) + Z2_vat_rev[yr] + Z2_tar_rev[yr])
                          #-(Z1_b_s_Z2[yr]*(Z2_xr[yr]-Z2_xr[yr-1])+Z1_b_cb_s_Z2[yr]*(Z2_xr[yr]-Z2_xr[yr-1]))
                          , digits = 2)
)

#Create table of results
Z2_GovDataTFM<-as.data.frame(Z2_Government,row.names=rownames)
kable(Z2_GovDataTFM)

#Create central bank aggregates
Z2_CentralBank <- c( 0,                                                                    
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     round(-Z2_xr[yr]*Z2_f_cb[yr], digits = 2),
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     round(Z2_xr[yr]*Z2_rb[yr-1]*Z2_b_cb[yr-1], digits = 2),
                     round(Z2_xr[yr]*(Z2_h_s[yr]-Z2_h_s[yr-1]), digits = 2),
                     round(Z2_xr[yr]*(-Z2_a_s[yr]+Z2_a_s[yr-1]), digits = 2),
                     0,
                     0,
                     0,
                     round(Z2_xr[yr]*(-Z2_b_cb[yr]+Z2_b_cb[yr-1]), digits = 2),
                     0,
                     0,
                     0,
                     round(Z2_xr[yr]*(-Z2_f_cb[yr]+Z2_rb[yr-1]*Z2_b_cb[yr-1]+
                             +(Z2_h_s[yr]-Z2_h_s[yr-1])
                             + Z2_xr[yr]*(-Z2_a_s[yr]+Z2_a_s[yr-1])
                           -(Z2_b_cb[yr]-Z2_b_cb[yr-1])) , digits = 2)
)

#Create table of results
Z2_CBDataTFM<-as.data.frame(Z2_CentralBank,row.names=rownames)
kable(Z2_CBDataTFM)

#Create banks aggregates
Z2_Banks       <- c( 0,                                                                    
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     round(-Z2_xr[yr]*Z2_f_b[yr], digits = 2),
                     0,
                     0,
                     0,
                     0,
                     round(-Z2_xr[yr]*Z2_rm[yr-1]*Z2_mh[yr-1], digits = 2),
                     round(Z2_xr[yr]*(Z2_rl[yr-1]*Z2_lf[yr-1]+Z2_rh[yr-1]*Z2_lh[yr-1]), digits = 2),
                     0,
                     round(Z2_xr[yr]*Z2_rb[yr-1]*Z2_b_b[yr-1], digits = 2),
                     0,
                     round(Z2_xr[yr]*(Z2_a_d[yr]-Z2_a_d[yr-1]), digits = 2),
                     round(Z2_xr[yr]*(Z2_mh[yr]-Z2_mh[yr-1]), digits = 2),
                     round(Z2_xr[yr]*(-Z2_ls[yr]+Z2_ls[yr-1]), digits = 2),
                     0,
                     round(Z2_xr[yr]*(-Z2_b_b[yr]+Z2_b_b[yr-1]), digits = 2),
                     0,
                     0,
                     0,
                     round(Z2_xr[yr]*(-Z2_f_b[yr] - Z2_rm[yr-1]*Z2_mh[yr-1] + Z2_rl[yr-1]*Z2_lf[yr-1] + Z2_rh[yr-1]*Z2_lh[yr-1] + Z2_rb[yr-1]*Z2_b_b[yr-1] +
                           + Z2_xr[yr]*(Z2_a_d[yr]-Z2_a_d[yr-1]) 
                           - (-Z2_mh[yr]+Z2_mh[yr-1] + Z2_ls[yr]-Z2_ls[yr-1] + Z2_b_b[yr]-Z2_b_b[yr-1]) )  , digits = 2)
)

#Create table of results
Z2_BanksDataTFM<-as.data.frame(Z2_Banks,row.names=rownames)
kable(Z2_BanksDataTFM)

################################################################################

#Create total aggregates
Total_TFM  <- c( 0,                                                                    
                 0,
                 0,
                 0,
                 0,
                 0,
                 0,
                 0,
                 0,
                 0,
                 0,
                 0,
                 0,
                 0,
                 0,
                 0,
                 round( Z1_rb[yr-1] * Z1_b_h_Z1[yr-1]+
                       -Z1_rb[yr-1] * Z1_b_s[yr-1]+
                        Z1_rb[yr-1] * Z1_b_cb[yr-1]+
                        Z1_rb[yr-1] * Z1_b_b[yr-1]+
                        Z1_rb[yr-1] * Z2_b_s_Z1[yr-1], digits=2),
                 
                 round( Z2_rb[yr-1] * Z1_b_s_Z2[yr-1] +
                        Z2_rb[yr-1] * Z1_b_cb_s_Z2[yr-1]+
                        Z2_rb[yr-1] * Z2_b_s_Z2[yr-1]+
                       -Z2_rb[yr-1] * Z2_b_s[yr-1]+
                        Z2_rb[yr-1] * Z2_b_cb[yr-1]+
                        Z2_rb[yr-1] * Z2_b_b[yr-1], digits=2),
                 0,
                 0,
                 0,
                 0,
                 round( -Z1_b_h_Z1[yr] + Z1_b_h_Z1[yr-1] +
                        +Z1_b_s[yr] - Z1_b_s[yr-1] +
                        -Z1_b_cb[yr] + Z1_b_cb[yr-1] +
                        -Z1_b_b[yr] + Z1_b_b[yr-1] +
                        -Z2_b_s_Z1[yr] + Z2_b_s_Z1[yr-1], digits=2),
                 
                 round( -Z2_b_h_Z2[yr] + Z2_b_h_Z2[yr-1] +
                        +Z2_b_s[yr] - Z2_b_s[yr-1] +
                        -Z2_b_cb[yr] + Z2_b_cb[yr-1] +
                        -Z2_b_b[yr] + Z2_b_b[yr-1] +
                        -Z1_b_s_Z2[yr] + Z1_b_s_Z2[yr-1] +
                        -Z1_b_cb_s_Z2[yr] + Z1_b_cb_s_Z2[yr-1], digits=2),
                 
                 round( -Z1_e_h_Z1[yr] + Z1_e_h_Z1[yr-1] +
                        +Z1_e_s[yr] - Z1_e_s[yr-1] +
                        -Z2_e_s_Z1[yr] + Z2_e_s_Z1[yr-1], digits=2),
                 
                 round( -Z2_e_h_Z2[yr] + Z2_e_h_Z2[yr-1] +
                        +Z2_e_s[yr] - Z2_e_s[yr-1] +
                        -Z1_e_s_Z2[yr] + Z1_e_s_Z2[yr-1], digits=2),
                 
                 round( Z2_xr[yr]*Z1_b_s_Z2[yr]*(Z2_xr[yr]-Z2_xr[yr-1])+
                        Z2_b_s_Z1[yr]*(Z1_xr[yr]-Z1_xr[yr-1]), digits = 2),
                 
                 round( Z1_rb[yr-1] * Z1_b_h_Z1[yr-1]+
                          -Z1_rb[yr-1] * Z1_b_s[yr-1]+
                          Z1_rb[yr-1] * Z1_b_cb[yr-1]+
                          Z1_rb[yr-1] * Z1_b_b[yr-1]+
                          Z1_rb[yr-1] * Z2_b_s_Z1[yr-1]+
                          Z2_rb[yr-1] * Z1_b_s_Z2[yr-1] +
                          Z2_rb[yr-1] * Z1_b_cb_s_Z2[yr-1]+
                          Z2_rb[yr-1] * Z2_b_s_Z2[yr-1]+
                          -Z2_rb[yr-1] * Z2_b_s[yr-1]+
                          Z2_rb[yr-1] * Z2_b_cb[yr-1]+
                          Z2_rb[yr-1] * Z2_b_b[yr-1]
                        -Z1_b_h_Z1[yr] + Z1_b_h_Z1[yr-1] +
                          +Z1_b_s[yr] - Z1_b_s[yr-1] +
                          -Z1_b_cb[yr] + Z1_b_cb[yr-1] +
                          -Z1_b_b[yr] + Z1_b_b[yr-1] +
                          -Z2_b_s_Z1[yr] + Z2_b_s_Z1[yr-1]
                        -Z2_b_h_Z2[yr] + Z2_b_h_Z2[yr-1] +
                          +Z2_b_s[yr] - Z2_b_s[yr-1] +
                          -Z2_b_cb[yr] + Z2_b_cb[yr-1] +
                          -Z2_b_b[yr] + Z2_b_b[yr-1] +
                          -Z1_b_s_Z2[yr] + Z1_b_s_Z2[yr-1] +
                          -Z1_b_cb_s_Z2[yr] + Z1_b_cb_s_Z2[yr-1] +
                          +Z2_xr[yr]*Z1_b_s_Z2[yr]*(Z2_xr[yr]-Z2_xr[yr-1])+
                          +Z2_b_s_Z1[yr]*(Z1_xr[yr]-Z1_xr[yr-1])
                        
                        -Z1_e_h_Z1[yr] + Z1_e_h_Z1[yr-1] +
                          +Z1_e_s[yr] - Z1_e_s[yr-1] +
                          -Z2_e_s_Z1[yr] + Z2_e_s_Z1[yr-1]
                        
                        -Z2_e_h_Z2[yr] + Z2_e_h_Z2[yr-1] +
                          +Z2_e_s[yr] - Z2_e_s[yr-1] +
                          -Z1_e_s_Z2[yr] + Z1_e_s_Z2[yr-1]
                        
                        , digits=2)
                        )

#Create table of results
TotDataTFM<-as.data.frame(Total_TFM,row.names=rownames)
kable(TotDataTFM)

#Create xr column
xr <- c( round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         paste("")
)        


#Create table of results
xrTFM<-as.data.frame(xr,row.names=rownames)
kable(xrTFM)

#Create TFM matrix
TFM_Matrix<-cbind(Z1_HouseDataTFM,Z1_FirmsDataTFM,Z1_CapitalDataTFM,Z1_GovDataTFM,Z1_BanksDataTFM,Z1_CBDataTFM,xr,Z2_HouseDataTFM,Z2_FirmsDataTFM,Z2_CapitalDataTFM,Z2_GovDataTFM,Z2_BanksDataTFM,Z2_CBDataTFM,TotDataTFM)
kable(TFM_Matrix) #Unload kableExtra to use this

################################################################################
################################################################################
################################################################################

#Create html version of tables

#Create titles
if (shock==1){title <- paste("increase in propensity to import of Area 2")}
if (shock==2){title <- paste("devaluation of Area 1 currency")}
if (shock==3){title <- paste("increase in propensity to import of Area 2 and quasi-floating exchange rate")}
if (shock==4){title <- paste("CE innovation in Area 1 with fixed exchange rate")}
if (shock==5){title <- paste("CE innovation in Area 1 with floating exchange rate")}
caption1 <- paste("Table 3. Balance sheet (OPEN) in period", yr) #"after",title
caption2 <- paste("Table 2. Transactions-flow matrix (OPEN) in period ",yr) #,"after",title


#Create html table for BS
BS_Matrix %>%
  kbl(caption=caption1,
      format= "html",
      #format= "latex",
      col.names = c("H1","F1","G1","B1","CB1","xr1","H2","F2","G2","B2","CB2","Tot"),
      align="r") %>%
  kable_classic(full_width = F, html_font = "helvetica")

#Create html table for TFM
TFM_Matrix %>%
  kbl(caption=caption2,
      format= "html",
      #format= "latex",
      col.names = c("H1","F1(curr)","F1(kap)","G1","B1","CB1","xr1","H2","F2(curr)","F2(kap)","G2","B2","CB2","Tot"),
      align="r") %>%
  kable_classic(full_width = F, html_font = "helvetica")


#See MRIO Table Code for IO matrix
