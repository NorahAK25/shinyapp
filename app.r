############################################################
# STAT413 STATISTICAL ANALYZER — Enhanced Charts Edition
# + Topic 6: Influence Diagnostics & Robust Regression
# + Topic 7: Polynomial Regression, Centering & Splines
############################################################

library(shiny)
library(bslib)
library(DT)
library(readxl)
library(corrplot)
library(RColorBrewer)
library(plotrix)
library(moments)
library(car)
library(lmtest)
library(EnvStats)
library(MASS)
library(olsrr)
library(glmnet)
library(boot)
library(splines)   # NEW — Topic 7

# ── Shared CSS ──────────────────────────────────────────
app_css <- "
@import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@300;400;500;600;700&family=DM+Mono:wght@400;500&display=swap');
*, *::before, *::after { box-sizing: border-box; }
body, .bslib-page-sidebar { background: #f0f4f1 !important; font-family: 'DM Sans', sans-serif !important; }
.sidebar { background: #1a2e1d !important; border-right: none !important; box-shadow: 4px 0 24px rgba(0,0,0,0.18) !important; padding: 0 !important; }
.sidebar-header { padding: 24px 20px 20px; border-bottom: 1px solid rgba(255,255,255,0.08); }
.sidebar-logo { display: flex; align-items: center; gap: 10px; margin-bottom: 4px; }
.sidebar-logo-icon { width: 34px; height: 34px; border-radius: 10px; background: linear-gradient(135deg, #4ade80, #16a34a); display: flex; align-items: center; justify-content: center; font-size: 16px; color: white; font-weight: 700; flex-shrink: 0; }
.sidebar-logo-text { font-size: 15px; font-weight: 700; color: #ffffff; letter-spacing: -0.02em; line-height: 1.2; }
.sidebar-logo-sub { font-size: 11px; color: rgba(255,255,255,0.45); font-weight: 400; margin-top: 2px; }
.sidebar-section { padding: 16px 12px 8px; }
.sidebar-section-label { font-size: 10px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.12em; color: rgba(255,255,255,0.35); padding: 0 8px; margin-bottom: 6px; }
.sidebar .nav-pills .nav-link { border-radius: 10px !important; color: rgba(255,255,255,0.6) !important; font-weight: 500 !important; font-size: 13.5px !important; padding: 9px 12px !important; margin-bottom: 2px !important; display: flex; align-items: center; gap: 8px; transition: all 0.15s ease; }
.sidebar .nav-pills .nav-link:hover { background: rgba(255,255,255,0.07) !important; color: rgba(255,255,255,0.9) !important; }
.sidebar .nav-pills .nav-link.active { background: rgba(74,222,128,0.18) !important; color: #4ade80 !important; font-weight: 600 !important; box-shadow: none !important; }
.upload-zone { background: rgba(255,255,255,0.05); border: 1.5px dashed rgba(255,255,255,0.2); border-radius: 12px; padding: 16px; margin: 12px; text-align: center; }
.upload-zone .form-label { color: rgba(255,255,255,0.7); font-size: 12px; }
.upload-zone .btn { background: #22c55e; border: none; color: white; font-size: 12px; }
.sidebar .form-control, .sidebar .form-select, .sidebar .selectize-input { background: rgba(255,255,255,0.08) !important; border: 1px solid rgba(255,255,255,0.15) !important; border-radius: 8px !important; color: white !important; font-size: 13px !important; }
.sidebar .selectize-dropdown { background: #1e3a22 !important; color: white !important; }
.sidebar .selectize-dropdown .option:hover { background: rgba(74,222,128,0.2) !important; }
.sidebar .form-label, .sidebar label { color: rgba(255,255,255,0.6) !important; font-size: 12px !important; }
.sidebar hr { border-color: rgba(255,255,255,0.1) !important; margin: 8px 12px; }
.sidebar .slider-animate-container { display: none; }
.sidebar .irs-line { background: rgba(255,255,255,0.15) !important; }
.sidebar .irs-bar { background: #22c55e !important; }
.sidebar .irs-handle > i { background: white !important; }
.sidebar .irs-from, .sidebar .irs-to, .sidebar .irs-single { background: #22c55e !important; }
.main-wrap { padding: 28px 32px; min-height: 100vh; }
.page-header { display: flex; align-items: baseline; gap: 12px; margin-bottom: 24px; }
.page-title { font-size: 26px; font-weight: 700; color: #0f1f12; letter-spacing: -0.03em; }
.page-subtitle { font-size: 13px; color: #6b7280; font-weight: 400; }
.stat-card { background: white; border-radius: 16px; border: 1px solid #e5ede8; padding: 20px; height: 100%; transition: box-shadow 0.2s; }
.stat-card:hover { box-shadow: 0 8px 32px rgba(22,101,52,0.10); }
.stat-card-label { font-size: 11px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.1em; color: #9ca3af; margin-bottom: 8px; }
.stat-card-value { font-size: 28px; font-weight: 700; color: #0f1f12; letter-spacing: -0.04em; }
.stat-card-badge { display: inline-block; background: #dcfce7; color: #166534; font-size: 11px; font-weight: 600; padding: 3px 10px; border-radius: 20px; margin-top: 6px; }
.card { background: white !important; border: 1px solid #e5ede8 !important; border-radius: 16px !important; box-shadow: 0 2px 12px rgba(22,101,52,0.04) !important; overflow: hidden; }
.card-header { background: white !important; border-bottom: 1px solid #f0f4f1 !important; color: #374151 !important; font-size: 12px !important; font-weight: 700 !important; text-transform: uppercase; letter-spacing: 0.08em; padding: 14px 18px !important; }
.analysis-tabs .nav-tabs { border-bottom: 2px solid #f0f4f1 !important; margin-bottom: 20px; gap: 4px; }
.analysis-tabs .nav-tabs .nav-link { border: none !important; border-radius: 8px 8px 0 0 !important; color: #6b7280 !important; font-size: 13px !important; font-weight: 500 !important; padding: 8px 16px !important; background: transparent !important; position: relative; bottom: -2px; }
.analysis-tabs .nav-tabs .nav-link:hover { color: #166534 !important; background: #f0fdf4 !important; }
.analysis-tabs .nav-tabs .nav-link.active { color: #166534 !important; font-weight: 700 !important; border-bottom: 2px solid #22c55e !important; background: transparent !important; }
.controls-bar { background: white; border: 1px solid #e5ede8; border-radius: 14px; padding: 16px 20px; margin-bottom: 20px; display: flex; flex-wrap: wrap; align-items: flex-end; gap: 16px; }
.controls-bar .form-label { font-size: 11px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.08em; color: #9ca3af; }
.controls-bar .form-control, .controls-bar .form-select { border: 1px solid #e5ede8; border-radius: 10px; font-size: 13px; }
pre.shiny-text-output { background: #f8faf9 !important; border: 1px solid #e5ede8 !important; border-radius: 12px !important; padding: 16px 18px !important; font-family: 'DM Mono', monospace !important; font-size: 12.5px !important; color: #1e293b !important; line-height: 1.65 !important; }
table.dataTable thead th { background: #f8faf9 !important; color: #374151 !important; font-size: 11px !important; font-weight: 700 !important; text-transform: uppercase; letter-spacing: 0.06em; border-bottom: 2px solid #e5ede8 !important; }
table.dataTable tbody tr:hover { background: #f0fdf4 !important; }
.empty-state { display: flex; flex-direction: column; align-items: center; justify-content: center; min-height: 320px; text-align: center; padding: 40px; }
.empty-state-icon { width: 64px; height: 64px; border-radius: 20px; background: linear-gradient(135deg, #dcfce7, #bbf7d0); display: flex; align-items: center; justify-content: center; font-size: 28px; margin-bottom: 16px; }
.empty-state-title { font-size: 18px; font-weight: 700; color: #0f1f12; margin-bottom: 8px; }
.empty-state-text  { font-size: 13.5px; color: #9ca3af; max-width: 280px; line-height: 1.6; }
.g-2col { display: grid; grid-template-columns: 1fr 1fr; gap: 16px; }
.g-3col { display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 16px; }
.g-1col { display: grid; grid-template-columns: 1fr; gap: 16px; }
.section-divider { font-size: 11px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.12em; color: #9ca3af; margin: 24px 0 12px; display: flex; align-items: center; gap: 10px; }
.section-divider::after { content: ''; flex: 1; height: 1px; background: #e5ede8; }
.bslib-page-title { display: none !important; }
.result-block { font-family: 'DM Sans', sans-serif; font-size: 13px; color: #1e293b; padding: 4px 0 8px 0; }
.rb-title { font-size: 11px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.1em; color: #6b7280; margin: 0 0 14px 0; padding-bottom: 10px; border-bottom: 1px solid #f0f4f1; }
.rb-section { font-size: 10px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.1em; color: #9ca3af; margin: 18px 0 8px 0; }
.rb-row { display: flex; justify-content: space-between; align-items: center; padding: 7px 12px; border-radius: 8px; transition: background 0.1s; }
.rb-row:hover { background: #f8faf9; }
.rb-key { font-size: 13px; color: #6b7280; flex: 1; }
.rb-val { font-size: 13px; font-weight: 600; color: #0f1f12; font-family: 'DM Mono', monospace; text-align: right; }
.rb-equation { background: #f0fdf4; border: 1px solid #bbf7d0; border-radius: 10px; padding: 12px 16px; font-family: 'DM Mono', monospace; font-size: 12.5px; color: #166534; margin-bottom: 12px; word-break: break-all; }
.rb-coef-table { width: 100%; border-collapse: collapse; font-size: 12.5px; margin-top: 4px; }
.rb-coef-table thead th { background: #f8faf9 !important; color: #9ca3af !important; font-size: 10px !important; font-weight: 700 !important; text-transform: uppercase !important; letter-spacing: 0.08em !important; padding: 6px 10px !important; border-bottom: 1px solid #e5ede8 !important; text-align: right; }
.rb-coef-table thead th:first-child { text-align: left; }
.rb-coef-table tbody tr { border-bottom: 1px solid #f8faf9; }
.rb-coef-table tbody tr:hover { background: #f0fdf4; }
.rb-coef-table tbody td { padding: 9px 10px; color: #374151; text-align: right; font-family: 'DM Mono', monospace; font-size: 12px; }
.rb-coef-table tbody td:first-child { text-align: left; font-family: 'DM Sans', sans-serif; font-weight: 500; color: #0f1f12; }
.sig-badge { display: inline-block; padding: 2px 8px; border-radius: 20px; font-size: 10px; font-weight: 700; font-family: 'DM Sans', sans-serif; white-space: nowrap; }
.sig-3star { background: #dcfce7; color: #166534; }
.sig-2star { background: #d1fae5; color: #065f46; }
.sig-1star { background: #fef9c3; color: #854d0e; }
.sig-dot   { background: #fef3c7; color: #92400e; }
.sig-ns    { background: #f3f4f6; color: #6b7280; }
.rb-decision { display: flex; align-items: center; gap: 10px; padding: 10px 14px; border-radius: 10px; font-size: 12.5px; font-weight: 600; margin-top: 8px; }
.rb-decision-sig   { background: #dcfce7; color: #166534; border: 1px solid #86efac; }
.rb-decision-ns    { background: #f3f4f6; color: #374151; border: 1px solid #d1d5db; }
.rb-decision-icon  { font-size: 14px; }
.rb-fit-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(110px, 1fr)); gap: 10px; margin-top: 10px; }
.rb-fit-cell { background: #f8faf9; border: 1px solid #e5ede8; border-radius: 10px; padding: 12px 14px; text-align: center; }
.rb-fit-label { font-size: 10px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.08em; color: #9ca3af; margin-bottom: 6px; }
.rb-fit-value { font-size: 15px; font-weight: 700; color: #0f1f12; font-family: 'DM Mono', monospace; }
.rb-anova-table { width: 100%; border-collapse: collapse; font-size: 12px; }
.rb-anova-table thead th { background: #f8faf9 !important; color: #9ca3af !important; font-size: 10px !important; font-weight: 700 !important; text-transform: uppercase !important; letter-spacing: 0.08em !important; padding: 6px 10px !important; border-bottom: 1px solid #e5ede8 !important; text-align: right; }
.rb-anova-table thead th:first-child { text-align: left; }
.rb-anova-table tbody tr { border-bottom: 1px solid #f8faf9; }
.rb-anova-table tbody tr:hover { background: #f0fdf4; }
.rb-anova-table tbody td { padding: 7px 10px; text-align: right; font-family: 'DM Mono', monospace; font-size: 12px; color: #374141; }
.rb-anova-table tbody td:first-child { text-align: left; font-family: 'DM Sans', sans-serif; font-weight: 600; color: #0f1f12; }
.rb-pred-table { width: 100%; border-collapse: collapse; font-size: 12px; }
.rb-pred-table thead th { background: #f8faf9 !important; color: #9ca3af !important; font-size: 10px !important; font-weight: 700 !important; text-transform: uppercase !important; letter-spacing: 0.08em !important; padding: 6px 10px !important; border-bottom: 1px solid #e5ede8 !important; text-align: right; }
.rb-pred-table thead th:first-child { text-align: left; }
.rb-pred-table tbody td { padding: 7px 10px; text-align: right; font-family: 'DM Mono', monospace; font-size: 12px; color: #374141; }
.rb-pred-table tbody td:first-child { text-align: left; font-family: 'DM Sans', sans-serif; font-weight: 500; color: #0f1f12; }
.rb-pred-table tbody tr { border-bottom: 1px solid #f8faf9; }
.rb-pred-table tbody tr:hover { background: #f0fdf4; }
.extrap-tag { background: #fef3c7; color: #92400e; font-size: 10px; font-weight: 700; padding: 1px 6px; border-radius: 6px; margin-left: 4px; }
.interp-tag { background: #dcfce7; color: #166534; font-size: 10px; font-weight: 700; padding: 1px 6px; border-radius: 6px; margin-left: 4px; }
.rb-test-strip { display: flex; align-items: center; justify-content: space-between; background: #f8faf9; border: 1px solid #e5ede8; border-radius: 10px; padding: 10px 14px; margin-bottom: 8px; }
.rb-test-name { font-size: 12px; font-weight: 700; color: #374151; }
.rb-test-stat { font-size: 12px; font-family: 'DM Mono', monospace; color: #6b7280; }
.rb-test-pval { font-size: 12px; font-family: 'DM Mono', monospace; font-weight: 600; color: #0f1f12; }
.rb-assumption { display: flex; align-items: center; gap: 8px; padding: 10px 14px; border-radius: 10px; border: 1px solid; margin-bottom: 8px; font-size: 12.5px; }
.rb-assumption-ok { background: #f0fdf4; border-color: #bbf7d0; color: #166534; }
.rb-assumption-violated { background: #fff7ed; border-color: #fed7aa; color: #9a3412; }
.rb-assumption-label { font-weight: 600; }
.rb-assumption-detail { font-size: 11.5px; opacity: 0.75; }
.rb-lambda-box { background: #f0fdf4; border: 1px solid #86efac; border-radius: 10px; padding: 10px 16px; display: flex; align-items: center; justify-content: space-between; margin-bottom: 12px; }
.rb-lambda-label { font-size: 11px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.08em; color: #6b7280; }
.rb-lambda-value { font-size: 20px; font-weight: 700; color: #166534; font-family: 'DM Mono', monospace; }
.rb-divider { height: 1px; background: #f0f4f1; margin: 16px 0; }
.rb-corr-row { display: flex; align-items: center; justify-content: space-between; padding: 7px 10px; border-radius: 8px; }
.rb-corr-row:hover { background: #f8faf9; }
.rb-corr-method { font-size: 12px; font-weight: 600; color: #374151; }
.rb-corr-val { font-family: 'DM Mono', monospace; font-size: 13px; font-weight: 700; color: #0f1f12; }
.rb-corr-bar-wrap { flex: 1; margin: 0 12px; height: 6px; background: #f0f4f1; border-radius: 3px; overflow: hidden; }
.rb-corr-bar { height: 6px; border-radius: 3px; }
.corr-pos { background: #22c55e; }
.corr-neg { background: #f97316; }
.rb-vif-row { display: flex; align-items: center; gap: 10px; padding: 8px 10px; border-radius: 8px; }
.rb-vif-row:hover { background: #f8faf9; }
.rb-vif-name { font-size: 12.5px; font-weight: 600; color: #374151; min-width: 100px; }
.rb-vif-bar-wrap { flex: 1; height: 8px; background: #f0f4f1; border-radius: 4px; overflow: hidden; }
.rb-vif-bar { height: 8px; border-radius: 4px; transition: width 0.4s; }
.vif-ok   { background: #22c55e; }
.vif-warn { background: #f59e0b; }
.vif-high { background: #ef4444; }
.rb-vif-val { font-family: 'DM Mono', monospace; font-size: 12px; font-weight: 700; color: #0f1f12; min-width: 40px; text-align: right; }
.landing-hero { background: linear-gradient(135deg, #0f1f12 0%, #1a3320 50%, #0f2d18 100%); border-radius: 20px; padding: 52px 48px 44px; margin-bottom: 28px; position: relative; overflow: hidden; }
.landing-hero::before { content: ''; position: absolute; top: -60px; right: -60px; width: 320px; height: 320px; border-radius: 50%; background: rgba(34,197,94,0.07); pointer-events: none; }
.landing-hero::after { content: ''; position: absolute; bottom: -80px; left: 20%; width: 240px; height: 240px; border-radius: 50%; background: rgba(74,222,128,0.05); pointer-events: none; }
.hero-eyebrow { display: inline-flex; align-items: center; gap: 8px; background: rgba(74,222,128,0.12); border: 1px solid rgba(74,222,128,0.25); border-radius: 20px; padding: 5px 14px; font-size: 11px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.12em; color: #4ade80; margin-bottom: 20px; }
.hero-dot { width: 6px; height: 6px; border-radius: 50%; background: #4ade80; animation: pulse-dot 2s ease-in-out infinite; }
@keyframes pulse-dot { 0%, 100% { opacity: 1; transform: scale(1); } 50% { opacity: 0.5; transform: scale(0.75); } }
.hero-title { font-size: 38px; font-weight: 700; color: #ffffff; letter-spacing: -0.04em; line-height: 1.12; margin-bottom: 16px; }
.hero-title-accent { color: #4ade80; }
.hero-desc { font-size: 14.5px; color: rgba(255,255,255,0.52); line-height: 1.75; max-width: 580px; margin-bottom: 0; }
.hero-stats-row { display: flex; gap: 36px; margin-top: 36px; padding-top: 28px; border-top: 1px solid rgba(255,255,255,0.08); }
.hero-stat { display: flex; flex-direction: column; gap: 3px; }
.hero-stat-value { font-size: 22px; font-weight: 700; color: #ffffff; letter-spacing: -0.03em; font-family: 'DM Mono', monospace; }
.hero-stat-label { font-size: 11px; color: rgba(255,255,255,0.38); font-weight: 500; text-transform: uppercase; letter-spacing: 0.08em; }
.features-grid { display: grid; grid-template-columns: repeat(4, 1fr); gap: 14px; margin-bottom: 28px; }
.feature-card { background: white; border: 1px solid #e5ede8; border-radius: 14px; padding: 20px 18px; transition: box-shadow 0.2s, transform 0.15s; }
.feature-card:hover { box-shadow: 0 6px 24px rgba(22,101,52,0.10); transform: translateY(-2px); }
.feature-icon { width: 38px; height: 38px; border-radius: 10px; display: flex; align-items: center; justify-content: center; font-size: 17px; margin-bottom: 12px; }
.fi-blue{background:#eff6ff;}.fi-green{background:#f0fdf4;}.fi-amber{background:#fffbeb;}.fi-purple{background:#faf5ff;}.fi-teal{background:#f0fdfa;}.fi-rose{background:#fff1f2;}.fi-indigo{background:#eef2ff;}.fi-orange{background:#fff7ed;}.fi-red{background:#fef2f2;}
.feature-name { font-size: 13px; font-weight: 700; color: #0f1f12; margin-bottom: 5px; }
.feature-desc { font-size: 12px; color: #9ca3af; line-height: 1.55; }
.datasets-grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 16px; margin-bottom: 28px; }
.dataset-card { background: white; border: 1.5px solid #e5ede8; border-radius: 16px; padding: 22px 20px; cursor: pointer; transition: all 0.18s ease; position: relative; overflow: hidden; }
.dataset-card::before { content: ''; position: absolute; top: 0; left: 0; right: 0; height: 3px; border-radius: 16px 16px 0 0; opacity: 0; transition: opacity 0.18s; }
.dataset-card:hover { border-color: #86efac; box-shadow: 0 8px 28px rgba(22,101,52,0.12); transform: translateY(-3px); }
.dataset-card:hover::before { opacity: 1; }
.ds-1::before{background:linear-gradient(90deg,#22c55e,#4ade80);}.ds-2::before{background:linear-gradient(90deg,#3b82f6,#60a5fa);}.ds-3::before{background:linear-gradient(90deg,#f59e0b,#fbbf24);}.ds-4::before{background:linear-gradient(90deg,#8b5cf6,#a78bfa);}.ds-5::before{background:linear-gradient(90deg,#ef4444,#f87171);}
.dataset-tag-row { display: flex; align-items: center; gap: 8px; margin-bottom: 12px; }
.dataset-tag { font-size: 10px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.1em; padding: 3px 9px; border-radius: 20px; }
.dt-green{background:#dcfce7;color:#166534;}.dt-blue{background:#dbeafe;color:#1e40af;}.dt-amber{background:#fef3c7;color:#92400e;}.dt-purple{background:#ede9fe;color:#5b21b6;}.dt-red{background:#fee2e2;color:#991b1b;}
.dataset-rows { font-size: 11px; color: #9ca3af; font-family: 'DM Mono', monospace; }
.dataset-name { font-size: 15px; font-weight: 700; color: #0f1f12; margin-bottom: 7px; letter-spacing: -0.02em; }
.dataset-desc { font-size: 12.5px; color: #6b7280; line-height: 1.6; margin-bottom: 14px; }
.dataset-vars { display: flex; flex-wrap: wrap; gap: 5px; margin-bottom: 16px; }
.dataset-var-pill { font-size: 11px; font-family: 'DM Mono', monospace; background: #f0f4f1; color: #374151; padding: 2px 8px; border-radius: 6px; border: 1px solid #e5ede8; }
.dataset-load-btn { display: flex; align-items: center; justify-content: center; gap: 6px; width: 100%; padding: 9px 16px; background: #f0fdf4; border: 1px solid #bbf7d0; border-radius: 10px; color: #166534; font-size: 12.5px; font-weight: 600; cursor: pointer; transition: background 0.15s, border-color 0.15s; }
.dataset-load-btn:hover { background: #dcfce7; border-color: #86efac; }
.dataset-card.ds-active { border-color: #22c55e; background: #f0fdf4; }
.dataset-card.ds-active .dataset-load-btn { background: #22c55e; color: white; border-color: #16a34a; }
.team-section { background: white; border: 1px solid #e5ede8; border-radius: 16px; padding: 24px 28px; margin-bottom: 8px; }
.team-header { display: flex; align-items: center; gap: 12px; margin-bottom: 20px; }
.team-header-label { font-size: 10px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.1em; color: #9ca3af; margin-bottom: 2px; }
.team-header-title { font-size: 14px; font-weight: 700; color: #0f1f12; }
.team-members-row { display: flex; gap: 10px; flex-wrap: wrap; }
.team-member { display: flex; align-items: center; gap: 10px; background: #f8faf9; border: 1px solid #e5ede8; border-radius: 12px; padding: 10px 14px; flex: 1; min-width: 155px; }
.member-avatar { width: 34px; height: 34px; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-size: 12px; font-weight: 700; flex-shrink: 0; }
.av-1{background:#dcfce7;color:#166534;}.av-2{background:#dbeafe;color:#1e40af;}.av-3{background:#fce7f3;color:#9d174d;}.av-4{background:#fef3c7;color:#92400e;}.av-5{background:#ede9fe;color:#5b21b6;}
.member-name { font-size: 12.5px; font-weight: 600; color: #0f1f12; line-height: 1.3; }
.member-role { font-size: 11px; color: #9ca3af; }
.section-label-row { display: flex; align-items: center; gap: 10px; margin-bottom: 14px; }
.section-label-text { font-size: 11px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.12em; color: #9ca3af; white-space: nowrap; }
.section-label-line { flex: 1; height: 1px; background: #e5ede8; }
"

# ── Sample Dataset Metadata ──────────────────────────────
sample_datasets <- list(
  topic0 = list(
    name="Topic 0 \u2014 Statistical Learning",tag="Statistical Learning",tag_cls="dt-blue",ds_cls="ds-1",
    rows="20 rows \u00b7 2 variables",desc="Introductory dataset for statistical learning concepts.",
    vars=c("Shear Strength (psi)","Age (weeks)"),path="topic-0.xlsx"),
  topic2 = list(
    name="Topic 2 \u2014 Simple Linear Regression",tag="SLR",tag_cls="dt-green",ds_cls="ds-2",
    rows="20 rows \u00b7 2 variables",desc="Rocket propellant shear strength as a function of propellant age.",
    vars=c("Shear Strength (psi)","Age (weeks)"),path="topic-2.xlsx"),
  topic3 = list(
    name="Topic 3 \u2014 Multiple Linear Regression",tag="MLR",tag_cls="dt-amber",ds_cls="ds-3",
    rows="25 rows \u00b7 3 variables",desc="Soft drink delivery time modeled by cases and distance.",
    vars=c("Delivery Time","Number of Cases","Distance (ft)"),path="topic-3.xlsx"),
  topic4 = list(
    name="Topic 4 \u2014 Model Adequacy",tag="Diagnostics",tag_cls="dt-purple",ds_cls="ds-4",
    rows="13 rows \u00b7 5 variables",desc="Hald\u2019s cement data \u2014 residual diagnostics.",
    vars=c("y","x1","x2","x3","x4"),path="topic-4.xlsx"),
  topic5 = list(
    name="Topic 5 \u2014 Correcting Model Adequacy",tag="Transformations",tag_cls="dt-green",ds_cls="ds-1",
    rows="16 rows \u00b7 5 variables",desc="Acetylene conversion data \u2014 Box-Cox and WLS.",
    vars=c("i","P","T","H","C"),path="topic-5.xlsx"),
  topic6 = list(
    name="Topic 6 \u2014 Influence & Robust Regression",tag="Influence \u00b7 Robust",tag_cls="dt-red",ds_cls="ds-5",
    rows="414 rows \u00b7 6 variables",desc="Taiwan real estate data \u2014 influence diagnostics and robust regression.",
    vars=c("y","x1","x2","x3","x4","x5"),path="topic-6.xlsx"),
  topic7_hw = list(
    name="Topic 7 \u2014 Hardwood (Polynomial)",tag="Polynomial",tag_cls="dt-green",ds_cls="ds-1",
    rows="15 rows \u00b7 2 variables",desc="Hardwood data \u2014 polynomial regression and centering.",
    vars=c("x","y"),path="topic-7-hardwood.xlsx"),
  topic7_vt = list(
    name="Topic 7 \u2014 Voltage Drop (Splines)",tag="Splines",tag_cls="dt-amber",ds_cls="ds-3",
    rows="41 rows \u00b7 2 variables",desc="Missile battery voltage drop \u2014 piecewise and spline regression.",
    vars=c("x (Time)","y (Voltage)"),path="topic-7-voltage.xlsx"),
  topic8 = list(
    name="Topic 8 \u2014 Indicator Variables",tag="Indicator \u00b7 Dummy",tag_cls="dt-blue",ds_cls="ds-2",
    rows="53 rows \u00b7 3 variables",desc="Tool life dataset for variable selection.",
    vars=c("i","y","x1","x2"),path="topic-8.xlsx"),
  topic9 = list(
    name="Topic 9 \u2014 Multicollinearity",tag="Multicollinearity",tag_cls="dt-amber",ds_cls="ds-3",
    rows="25 rows \u00b7 3 variables",desc="Wind mill DC output \u2014 VIF, ridge, LASSO diagnostics.",
    vars=c("i","x","y"),path="topic-9.xlsx"),
  topic10 = list(
    name="Topic 10 \u2014 Model Building",tag="Variable Selection",tag_cls="dt-purple",ds_cls="ds-4",
    rows="20 rows \u00b7 4 variables",desc="Tool life dataset for variable selection.",
    vars=c("i","y","x1","x2"),path="topic-10.xlsx"),
  topic11_binary = list(
    name="Topic 11 \u2014 Logit / Probit",tag="Binary GLM",tag_cls="dt-blue",ds_cls="ds-2",
    rows="196 rows \u00b7 2 variables",desc="Wellness dataset \u2014 binary outcome modeled by work hours.",
    vars=c("wellness (0/1)","work"),path="topic-11-logit-probit.xlsx"),
  topic11_poisson = list(
    name="Topic 11 \u2014 Poisson",tag="Count GLM",tag_cls="dt-red",ds_cls="ds-5",
    rows="200 rows \u00b7 3 variables",desc="Awards dataset \u2014 count modeled by math score and program.",
    vars=c("num_awards","math","prog"),path="topic-11-poisson.csv"),
  topic12_bayes = list(
    name="Bayesian Modeling \u2014 Rocket Propellant",tag="Bayesian",tag_cls="dt-purple",ds_cls="ds-4",
    rows="20 rows \u00b7 2 variables",desc="Rocket propellant shear strength with classical OLS and Bayesian linear regression.",
    vars=c("Shear Strength (psi)","Age (weeks)"),path="P1_RocketPropellant.xlsx")
)

# ── HTML Builder Helpers ─────────────────────────────────
sig_class <- function(p) {
  if (p < 0.001) "sig-3star" else if (p < 0.01) "sig-2star" else if (p < 0.05) "sig-1star" else if (p < 0.1) "sig-dot" else "sig-ns"
}
sig_label <- function(p) {
  if (p < 0.001) "p < 0.001 ***" else if (p < 0.01) paste0("p = ",formatC(p,4,format="f")," **") else if (p < 0.05) paste0("p = ",formatC(p,4,format="f")," *") else if (p < 0.1) paste0("p = ",formatC(p,4,format="f")," .") else paste0("p = ",formatC(p,4,format="f")," ns")
}
decision_html <- function(p, alpha = 0.05) {
  if (p < alpha) sprintf('<div class="rb-decision rb-decision-sig"><span class="rb-decision-icon">\u2713</span> Significant \u2014 p = %s (reject H\u2080 at \u03b1 = %s)</div>', formatC(p,4,format="e"), alpha)
  else sprintf('<div class="rb-decision rb-decision-ns"><span class="rb-decision-icon">\u25cb</span> Not significant \u2014 p = %s (fail to reject H\u2080)</div>', formatC(p,4,format="e"))
}
fmt_num <- function(x, d = 4) formatC(round(x, d), digits = d, format = "f")

coef_table_html <- function(s, term_names) {
  rows <- ""
  for (i in seq_len(nrow(s$coefficients))) {
    est <- s$coefficients[i,1]; se <- s$coefficients[i,2]
    tv  <- s$coefficients[i,3]
    pv  <- if (ncol(s$coefficients) >= 4) s$coefficients[i,4] else NA
    if (!is.na(pv)) {
      sc <- sig_class(pv); sl <- sig_label(pv)
      rows <- paste0(rows, sprintf('<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td><span class="sig-badge %s">%s</span></td></tr>',
                                   term_names[i], fmt_num(est), fmt_num(se), fmt_num(tv,3), sc, sl))
    } else {
      rows <- paste0(rows, sprintf('<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td><span class="sig-badge sig-ns">\u2014</span></td></tr>',
                                   term_names[i], fmt_num(est), fmt_num(se), fmt_num(tv,3)))
    }
  }
  sprintf('<table class="rb-coef-table"><thead><tr><th>Term</th><th>Estimate</th><th>Std. Error</th><th>t-value</th><th>Significance</th></tr></thead><tbody>%s</tbody></table>', rows)
}

fit_stats_html <- function(s) {
  if (is.null(s$fstatistic)) {
    sprintf('<div class="rb-fit-grid"><div class="rb-fit-cell"><div class="rb-fit-label">Residual SE</div><div class="rb-fit-value">%s</div></div></div>', fmt_num(s$sigma,4))
  } else {
    fp <- pf(s$fstatistic[1],s$fstatistic[2],s$fstatistic[3],lower.tail=FALSE); sc <- sig_class(fp)
    sprintf('<div class="rb-fit-grid"><div class="rb-fit-cell"><div class="rb-fit-label">R\u00b2</div><div class="rb-fit-value">%s</div></div><div class="rb-fit-cell"><div class="rb-fit-label">Adj. R\u00b2</div><div class="rb-fit-value">%s</div></div><div class="rb-fit-cell"><div class="rb-fit-label">Residual SE</div><div class="rb-fit-value">%s</div></div><div class="rb-fit-cell"><div class="rb-fit-label">F-statistic</div><div class="rb-fit-value"><span class="sig-badge %s">%s</span></div></div></div>',
            fmt_num(s$r.squared,4), fmt_num(s$adj.r.squared,4), fmt_num(s$sigma,4), sc, sig_label(fp))
  }
}

anova_table_html <- function(atab, row_names) {
  rows <- ""
  for (i in seq_len(nrow(atab))) {
    df <- atab[i,"Df"]; ss <- atab[i,"Sum Sq"]; ms <- atab[i,"Mean Sq"]
    fv <- atab[i,"F value"]; pv <- atab[i,"Pr(>F)"]
    if (!is.na(fv) && !is.na(pv)) {
      sc <- sig_class(pv); sl <- sig_label(pv)
      rows <- paste0(rows, sprintf('<tr><td>%s</td><td>%d</td><td>%s</td><td>%s</td><td>%s</td><td><span class="sig-badge %s">%s</span></td></tr>',
                                   row_names[i], df, fmt_num(ss,2), fmt_num(ms,2), fmt_num(fv,3), sc, sl))
    } else {
      rows <- paste0(rows, sprintf('<tr><td>%s</td><td>%d</td><td>%s</td><td>%s</td><td>\u2014</td><td>\u2014</td></tr>',
                                   row_names[i], df, fmt_num(ss,2), fmt_num(ms,2)))
    }
  }
  sprintf('<table class="rb-anova-table"><thead><tr><th>Source</th><th>df</th><th>SS</th><th>MS</th><th>F</th><th>Significance</th></tr></thead><tbody>%s</tbody></table>', rows)
}

ci_table_html <- function(ci, row_names, pct) {
  rows <- ""
  for (i in seq_len(nrow(ci)))
    rows <- paste0(rows, sprintf('<tr><td>%s</td><td>%s</td><td>%s</td></tr>', row_names[i], fmt_num(ci[i,1],5), fmt_num(ci[i,2],5)))
  sprintf('<div class="rb-section">%d%% Confidence Intervals</div><table class="rb-pred-table"><thead><tr><th>Term</th><th>Lower</th><th>Upper</th></tr></thead><tbody>%s</tbody></table>', pct, rows)
}

pred_table_html <- function(out, xr = NULL, label = "Fit") {
  rows <- ""
  for (i in seq_len(nrow(out))) {
    tag <- ""
    if (!is.null(xr) && !is.na(xr[1])) {
      rn <- rownames(out)[i]
      xval <- suppressWarnings(as.numeric(gsub("[^0-9.\\-]","",regmatches(rn,regexpr("[-0-9.]+",rn)))))
      if (!is.na(xval) && (xval < xr[1] || xval > xr[2])) tag <- '<span class="extrap-tag">Extrap</span>'
      else tag <- '<span class="interp-tag">Interp</span>'
    }
    rows <- paste0(rows, sprintf('<tr><td>%s %s</td><td>%s</td><td>%s</td><td>%s</td></tr>',
                                 rownames(out)[i], tag, fmt_num(out[i,"fit"]), fmt_num(out[i,"lwr"]), fmt_num(out[i,"upr"])))
  }
  sprintf('<table class="rb-pred-table"><thead><tr><th>Point</th><th>Fit</th><th>Lower</th><th>Upper</th></tr></thead><tbody>%s</tbody></table>', rows)
}

assumption_html <- function(label, ok, detail) {
  cls <- if (ok) "rb-assumption-ok" else "rb-assumption-violated"
  icon <- if (ok) "\u2713" else "!"
  sprintf('<div class="rb-assumption %s"><span style="font-weight:700;font-size:14px;">%s</span><span class="rb-assumption-label">%s</span><span class="rb-assumption-detail">\u2014 %s</span></div>', cls, icon, label, detail)
}

model_summary_html <- function(m, s, term_names, y_name, x_names, subtitle = NULL) {
  b <- coef(m)
  terms_eq <- paste(vapply(seq_along(x_names), function(i) sprintf("<b>%.4f</b> \u00d7 %s", b[i+1], x_names[i]), character(1)), collapse=" + ")
  eq_str <- sprintf("%s = %.4f + %s", y_name, b[1], terms_eq)
  sub_html <- if (!is.null(subtitle)) sprintf('<div style="font-size:11.5px;color:#9ca3af;margin-bottom:10px;">%s</div>', subtitle) else ""
  paste0('<div class="result-block">', sub_html, '<div class="rb-section">Model Equation</div><div class="rb-equation">', eq_str, '</div><div class="rb-section">Coefficients</div>', coef_table_html(s, term_names), '<div class="rb-section">Fit Statistics</div>', fit_stats_html(s), '</div>')
}

# ── Theme ────────────────────────────────────────────────
APP_GREEN  <- "#22c55e"; APP_GREEN2 <- "#4ade80"; APP_DARK   <- "#166534"
APP_BLUE   <- "#3b82f6"; APP_ORANGE <- "#f97316"; APP_PURPLE <- "#8b5cf6"
APP_RED    <- "#ef4444"; APP_GRAY   <- "#9ca3af"; APP_BG     <- "#f8faf9"; APP_GRID   <- "#e5ede8"

app_theme <- function() {
  par(bg="#ffffff", col.main="#0f1f12", col.lab="#374151", col.axis="#6b7280",
      family="sans", font.main=2, cex.main=0.92, cex.lab=0.80, cex.axis=0.72,
      tcl=-0.25, mgp=c(2.2,0.45,0), mar=c(3.8,3.8,3.2,1.4),
      las=1, lend="round", ljoin="round")
}
add_grid <- function(nx=NULL, ny=NULL, col=APP_GRID) { grid(nx=nx,ny=ny,col=col,lty=1,lwd=0.6); box(col=APP_GRID,lwd=0.8) }
make_palette <- function(n, palette_name="Set2") {
  base_n <- min(max(n,3),8); base_cols <- brewer.pal(base_n, palette_name)
  if (n <= length(base_cols)) base_cols[seq_len(n)] else colorRampPalette(base_cols)(n)
}

# ════════════════════════════════════════════
# TOPIC 7 PLOT HELPER FUNCTIONS  (NEW)
# ════════════════════════════════════════════

poly_resid_plot <- function(m, main_str) {
  app_theme()
  fi <- fitted(m); ei <- residuals(m); sg <- sd(ei)
  plot(fi, ei, type="n", xlab="Fitted Values", ylab="Residuals", main=main_str)
  add_grid()
  abline(h=0, col=APP_GRAY, lwd=1.5, lty=2)
  abline(h= 2*sg, col=APP_ORANGE, lwd=1, lty=3)
  abline(h=-2*sg, col=APP_ORANGE, lwd=1, lty=3)
  col_pts <- ifelse(abs(ei) > 2*sg, APP_RED, adjustcolor(APP_PURPLE, 0.65))
  points(fi, ei, pch=21, bg=col_pts, col="white", cex=0.82)
  lo <- loess(ei ~ fi); xo <- sort(fi); lines(xo, predict(lo, xo), col=APP_RED, lwd=1.8)
  mtext("LOESS trend in red  |  \u00b12\u03c3 bands", side=3, line=0.1, cex=0.67, col="#9ca3af")
}

vif_barplot_t7 <- function(vif_vals, title_str) {
  app_theme()
  par(mar=c(5, 3.8, 3.2, 1.4))
  cols <- ifelse(vif_vals < 5, APP_GREEN, ifelse(vif_vals < 10, "#f59e0b", APP_RED))
  ymax <- max(c(vif_vals, 11)) * 1.08
  bp <- barplot(vif_vals, col=cols, border=NA, las=2, cex.names=0.78,
                ylim=c(0, ymax), main=title_str, ylab="VIF")
  add_grid(nx=NA)
  abline(h=5,  col=APP_RED,    lwd=1.6, lty=3)
  abline(h=10, col=APP_ORANGE, lwd=1.4, lty=3)
  text(bp, vif_vals + ymax*0.025,
       labels=formatC(vif_vals, format="f", digits=1),
       cex=0.72, col="#0f1f12", font=2)
  legend("topright",
         legend=c("VIF < 5", "5\u201310", "VIF \u226510", "Warn=5", "Severe=10"),
         col=c(APP_GREEN,"#f59e0b",APP_RED,APP_RED,APP_ORANGE),
         pch=c(15,15,15,NA,NA), lty=c(NA,NA,NA,3,3),
         bty="n", cex=0.68, text.col="#374151")
}

poly_model_summary_html <- function(m, label) {
  s <- summary(m)
  terms <- rownames(s$coefficients)
  HTML(paste0(
    '<div class="result-block">',
    '<div style="font-size:11px;color:#9ca3af;margin-bottom:8px;">', label, '</div>',
    '<div class="rb-section">Coefficients</div>',
    coef_table_html(s, terms),
    '<div class="rb-section">Fit Statistics</div>',
    fit_stats_html(s), '</div>'))
}
# ── UI ─────────────────────────────────────────────────
ui <- page_sidebar(
  title = NULL,
  theme = bs_theme(version=5, primary="#22c55e", secondary="#ecfdf5", base_font=font_google("DM Sans")),
  tags$head(
    tags$style(HTML(app_css)),
    tags$script(HTML(
      "$(document).on('click', '.dataset-load-btn', function() {
         $('.dataset-card').removeClass('ds-active');
         $(this).closest('.dataset-card').addClass('ds-active');
         var btn = $(this);
         btn.html('&#10003; Loaded');
         setTimeout(function() { btn.html('&#10003; Active Dataset'); }, 900);
       });"
    ))
  ),
  sidebar = sidebar(
    width = 260, open = TRUE,
    div(class = "sidebar-header",
        div(style = "text-align:center; padding-bottom:10px;",
            tags$img(src="kfupm-logo.png", style="width:80px; height:80px; object-fit:contain;")),
        div(class = "sidebar-logo",
            div(class = "sidebar-logo-icon", "\u25c6"),
            div(div(class = "sidebar-logo-text", "STAT 413"),
                div(class = "sidebar-logo-sub", "Statistical Analyzer")))),
    div(class = "upload-zone",
        fileInput("file", "Upload CSV or Excel", accept=c(".csv",".xlsx",".xls"),
                  buttonLabel="Browse", placeholder="No file"),
        uiOutput("sheet_ui")),
    div(class = "sidebar-section",
        div(class = "sidebar-section-label", "Analysis"),
        navset_pill_list(
          id = "main_nav", widths = c(12,12), selected = "home",
          nav_panel("  Home",                  value = "home"),
          nav_panel("  Overview",              value = "overview"),
          nav_panel("  Single Numeric",        value = "single_num"),
          nav_panel("  Two Numeric",           value = "two_num"),
          nav_panel("  Multi Numeric",         value = "multi_num"),
          nav_panel("  Categorical",           value = "categorical"),
          nav_panel("  Hypothesis Tests",      value = "hypo"),
          nav_panel("  Regression",            value = "regression"),
          nav_panel("  Transformations",       value = "transforms"),
          nav_panel("  Influence & Robust",    value = "influence"),
          nav_panel("  Feature Selection",     value = "feature_selection"),
          nav_panel("  Multicollinearity",     value = "multicol"),
          nav_panel("  Polynomial & Splines",  value = "topic7"),   # NEW
          nav_panel("  Bayesian Modeling",     value = "bayesian"),
          nav_panel("  GLM",                   value = "glm")
        ))
  ),
  
  div(class = "main-wrap",
      
      # ── HOME ──────────────────────────────────────────────
      conditionalPanel("input.main_nav === 'home'",
                       div(class = "landing-hero",
                           div(class = "hero-eyebrow", div(class = "hero-dot"), "STAT 413 \u00b7 Statistical Analysis Tool"),
                           div(class = "hero-title", "Analyze Data.", tags$br(),
                               tags$span(class="hero-title-accent", "Understand Statistics.")),
                           div(class = "hero-desc",
                               "A complete statistical analysis environment built for STAT 413. Explore distributions, run hypothesis tests, fit regression models, apply transformations, diagnose influence and multicollinearity, fit polynomial and spline models, and interpret results \u2014 all through a clean, interactive interface."),
                           div(class = "hero-stats-row",
                               div(class="hero-stat", div(class="hero-stat-value","13"), div(class="hero-stat-label","Analysis Modules")),
                               div(class="hero-stat", div(class="hero-stat-value","40+"),div(class="hero-stat-label","Statistical Methods")),
                               div(class="hero-stat", div(class="hero-stat-value","14"), div(class="hero-stat-label","Sample Datasets")),
                               div(class="hero-stat", div(class="hero-stat-value","CSV"),div(class="hero-stat-label","& Excel Support")))),
                       div(class="section-label-row",
                           div(class="section-label-text","What you can do"),
                           div(class="section-label-line")),
                       div(class="features-grid",
                           div(class="feature-card",div(class="feature-icon fi-blue","\U0001f4ca"),
                               div(class="feature-name","Descriptive Statistics"),
                               div(class="feature-desc","Mean, median, variance, skewness, kurtosis, and visual summaries.")),
                           div(class="feature-card",div(class="feature-icon fi-green","\U0001f50d"),
                               div(class="feature-name","Hypothesis Testing"),
                               div(class="feature-desc","One-sample t-test, two-sample t-test, Wilcoxon, and Mann-Whitney.")),
                           div(class="feature-card",div(class="feature-icon fi-purple","\U0001f4c8"),
                               div(class="feature-name","Simple & Multiple Regression"),
                               div(class="feature-desc","SLR and MLR with ANOVA tables, CIs, prediction intervals, and diagnostic plots.")),
                           div(class="feature-card",div(class="feature-icon fi-amber","\U0001f3af"),
                               div(class="feature-name","Indicator Variables"),
                               div(class="feature-desc","Model group effects with dummy variables across three nested models.")),
                           div(class="feature-card",div(class="feature-icon fi-teal","\U0001f501"),
                               div(class="feature-name","Box-Cox & Box-Tidwell"),
                               div(class="feature-desc","Find optimal \u03bb and \u03b1 transformations to correct non-linearity.")),
                           div(class="feature-card",div(class="feature-icon fi-orange","\u2696\ufe0f"),
                               div(class="feature-name","Weighted Least Squares"),
                               div(class="feature-desc","Compare OLS and WLS when variance is non-constant.")),
                           div(class="feature-card",div(class="feature-icon fi-red","\U0001f6e1\ufe0f"),
                               div(class="feature-name","Influence & Robust Regression"),
                               div(class="feature-desc","Cook's distance, DFFITS, leverage, Huber and bisquare robust regression.")),
                           div(class="feature-card",div(class="feature-icon fi-rose","\U0001f9ee"),
                               div(class="feature-name","Categorical Analysis"),
                               div(class="feature-desc","Frequency tables, proportions, contingency tables, mosaic plots.")),
                           div(class="feature-card",div(class="feature-icon fi-indigo","\U0001f9f1"),
                               div(class="feature-name","Multicollinearity & Ridge/LASSO"),
                               div(class="feature-desc","VIFs, centering, ridge, lasso, and 10-fold CV-RMSE comparison.")),
                           div(class="feature-card",div(class="feature-icon fi-green","\U0001f4cf"),
                               div(class="feature-name","Polynomial & Spline Regression"),
                               div(class="feature-desc","Linear through quartic models, centering to fix multicollinearity, B-splines with adjustable knots.")),
                           div(class="feature-card",div(class="feature-icon fi-teal","\U0001f4ca"),
                               div(class="feature-name","Generalized Linear Models"),
                               div(class="feature-desc","Logistic (logit), probit, and Poisson regression with deviance tests.")),
                           div(class="feature-card",div(class="feature-icon fi-purple","\U0001f52c"),
                               div(class="feature-name","Bayesian Modeling"),
                               div(class="feature-desc","Compare OLS estimates with posterior summaries, credible intervals, and posterior probabilities."))),
                       div(class="section-label-row",
                           div(class="section-label-text","Sample Datasets \u2014 click to load"),
                           div(class="section-label-line")),
                       div(class="datasets-grid",
                           lapply(names(sample_datasets), function(key) {
                             ds <- sample_datasets[[key]]
                             var_pills <- paste(sapply(ds$vars, function(v)
                               sprintf('<span class="dataset-var-pill">%s</span>', v)), collapse="")
                             div(class=paste("dataset-card", ds$ds_cls), id=paste0("ds_card_",key),
                                 div(class="dataset-tag-row",
                                     span(class=paste("dataset-tag",ds$tag_cls), ds$tag),
                                     span(class="dataset-rows", ds$rows)),
                                 div(class="dataset-name", ds$name),
                                 div(class="dataset-desc",  ds$desc),
                                 div(class="dataset-vars",  HTML(var_pills)),
                                 div(class="dataset-load-btn", id=paste0("load_",key),
                                     onclick=sprintf("Shiny.setInputValue('load_dataset','%s',{priority:'event'})", key),
                                     "\u2193  Load Dataset"))
                           })),
                       div(class="section-label-row",
                           div(class="section-label-text","Project Team"),
                           div(class="section-label-line")),
                       div(class="team-section",
                           div(class="team-header",
                               div(style="width:38px;height:38px;border-radius:10px;background:#f0fdf4;border:1px solid #bbf7d0;display:flex;align-items:center;justify-content:center;font-size:16px;","\U0001f393"),
                               div(div(class="team-header-label","STAT 413 \u00b7 King Fahd University of Petroleum & Minerals"),
                                   div(class="team-header-title","Team 1"))),
                           div(class="team-members-row",
                               div(class="team-member",div(class="member-avatar av-1","AA"),div(div(class="member-name","Aryam Alshehri"),  div(class="member-role","Team Member"))),
                               div(class="team-member",div(class="member-avatar av-2","JA"),div(div(class="member-name","Jude Alharbi"),    div(class="member-role","Team Member"))),
                               div(class="team-member",div(class="member-avatar av-3","ZA"),div(div(class="member-name","Zahraa Alhaddab"), div(class="member-role","Team Member"))),
                               div(class="team-member",div(class="member-avatar av-4","LA"),div(div(class="member-name","Lamees Alikhwan"), div(class="member-role","Team Member"))),
                               div(class="team-member",div(class="member-avatar av-5","NA"),div(div(class="member-name","Norah Alkhalifah"),div(class="member-role","Team Member")))))
      ),
      
      # ── OVERVIEW ──────────────────────────────────────────
      conditionalPanel("input.main_nav === 'overview'",
                       div(class="page-header",
                           div(class="page-title","Data Overview"),
                           div(class="page-subtitle","Upload a file to begin your analysis")),
                       uiOutput("overview_stats"),
                       div(class="section-divider","Preview"),
                       div(class="card",
                           div(class="card-header","Dataset Preview"),
                           div(style="padding:0;", DTOutput("preview"))),
                       uiOutput("hist_controls_ui")
      ),
      
      # ── SINGLE NUMERIC ────────────────────────────────────
      conditionalPanel("input.main_nav === 'single_num'",
                       div(class="page-header",
                           div(class="page-title","Single Numeric"),
                           div(class="page-subtitle","Distribution, summary stats, and shape")),
                       div(class="controls-bar",
                           div(selectInput("num_var","Variable",NULL,width="200px")),
                           div(selectInput("single_view","View",
                                           choices=c("Histogram","Boxplot","Summary"),
                                           selected="Histogram",width="180px"))),
                       conditionalPanel("input.single_view == 'Histogram'",
                                        div(class="card",div(class="card-header","Histogram with Density"),
                                            div(style="padding:12px;",plotOutput("hist",height=320)))),
                       conditionalPanel("input.single_view == 'Boxplot'",
                                        div(class="card",div(class="card-header","Boxplot"),
                                            div(style="padding:12px;",plotOutput("box",height=320)))),
                       conditionalPanel("input.single_view == 'Summary'",
                                        div(class="card",div(class="card-header","Summary Table"),
                                            div(style="padding:12px;",DTOutput("desc"))))
      ),
      
      # ── TWO NUMERIC ───────────────────────────────────────
      conditionalPanel("input.main_nav === 'two_num'",
                       div(class="page-header",
                           div(class="page-title","Two Numeric"),
                           div(class="page-subtitle","Correlation and scatter analysis")),
                       div(class="controls-bar",
                           div(selectInput("num_x","X Variable",NULL,width="200px")),
                           div(selectInput("num_y","Y Variable",NULL,width="200px")),
                           div(selectInput("two_view","View",c("Scatter","Correlation"),
                                           selected="Scatter",width="160px"))),
                       conditionalPanel("input.two_view=='Scatter'",
                                        div(class="card",div(class="card-header","Scatter Plot"),
                                            div(style="padding:12px;",plotOutput("scatter",height=350)))),
                       conditionalPanel("input.two_view=='Correlation'",
                                        div(class="card",div(class="card-header","Correlation Table"),
                                            div(style="padding:12px;",DTOutput("corr"))))
      ),
      
      # ── MULTI NUMERIC ─────────────────────────────────────
      conditionalPanel("input.main_nav === 'multi_num'",
                       div(class="page-header",
                           div(class="page-title","Multiple Numeric"),
                           div(class="page-subtitle","Correlation matrix and pairs plot")),
                       div(class="controls-bar",
                           div(selectInput("multi_view","View",
                                           c("Correlation Matrix","Pairs Plot"),
                                           selected="Correlation Matrix",width="220px"))),
                       conditionalPanel("input.multi_view=='Correlation Matrix'",
                                        div(class="card",div(class="card-header","Correlation Matrix"),
                                            div(style="padding:12px;",plotOutput("corr_matrix",height=420)))),
                       conditionalPanel("input.multi_view=='Pairs Plot'",
                                        div(class="card",div(class="card-header","Pairs Plot"),
                                            div(style="padding:12px;",plotOutput("pairs_plot",height=420))))
      ),# ── CATEGORICAL ───────────────────────────────────────
      conditionalPanel("input.main_nav === 'categorical'",
                       div(class="page-header",
                           div(class="page-title","Categorical Analysis"),
                           div(class="page-subtitle","Tables and charts for category variables")),
                       div(class="analysis-tabs",
                           tabsetPanel(id="cat_tabs", type="tabs",
                                       tabPanel("Single Variable", br(),
                                                div(class="controls-bar",
                                                    div(selectInput("cat_var","Category Variable",NULL,width="220px"))),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Frequency Table"),
                                                        div(style="padding:4px;",DTOutput("freq_table"))),
                                                    div(class="card",div(class="card-header","Proportion Table"),
                                                        div(style="padding:4px;",DTOutput("prop_table")))),
                                                div(class="section-divider","Charts"),
                                                div(class="g-3col",
                                                    div(class="card",div(class="card-header","Bar Plot"),
                                                        div(style="padding:12px;",plotOutput("bar",height=240))),
                                                    div(class="card",div(class="card-header","Pie Chart"),
                                                        div(style="padding:12px;",plotOutput("pie",height=240))),
                                                    div(class="card",div(class="card-header","3D Pie"),
                                                        div(style="padding:12px;",plotOutput("pie3d",height=240))))
                                       ),
                                       tabPanel("Two Variables", br(),
                                                div(class="controls-bar",
                                                    div(selectInput("cat_a","Category A",NULL,width="200px")),
                                                    div(selectInput("cat_b","Category B",NULL,width="200px"))),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Contingency Table"),
                                                        div(style="padding:4px;",DTOutput("cont_table"))),
                                                    div(class="card",div(class="card-header","Joint Proportions"),
                                                        div(style="padding:4px;",DTOutput("joint_table")))),
                                                div(class="section-divider","Charts"),
                                                div(class="g-3col",
                                                    div(class="card",div(class="card-header","Stacked Bar"),
                                                        div(style="padding:12px;",plotOutput("stacked",height=240))),
                                                    div(class="card",div(class="card-header","Side-by-Side Bar"),
                                                        div(style="padding:12px;",plotOutput("side",height=240))),
                                                    div(class="card",div(class="card-header","Mosaic Plot"),
                                                        div(style="padding:12px;",plotOutput("mosaic",height=240))))
                                       ),
                                       tabPanel("Three Variables", br(),
                                                div(class="controls-bar",
                                                    div(selectInput("cat_a3","Category A",NULL,width="180px")),
                                                    div(selectInput("cat_b3","Category B",NULL,width="180px")),
                                                    div(selectInput("cat_c3","Category C",NULL,width="180px"))),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Multi-way Table"),
                                                        div(style="padding:4px;",DTOutput("multi_table"))),
                                                    div(class="card",div(class="card-header","Flat Table"),
                                                        div(style="padding:4px;",DTOutput("flat_table")))),
                                                div(class="section-divider","Chart"),
                                                div(class="card",div(class="card-header","Three-way Bar"),
                                                    div(style="padding:12px;",plotOutput("three_bar",height=320)))
                                       )
                           ))
      ),
      
      # ── HYPOTHESIS TESTS ──────────────────────────────────
      conditionalPanel("input.main_nav === 'hypo'",
                       div(class="page-header",
                           div(class="page-title","Hypothesis Testing"),
                           div(class="page-subtitle","One-sample and two-sample tests")),
                       div(class="controls-bar",
                           div(selectInput("test_var","Numeric Variable",NULL,width="200px")),
                           div(selectInput("gender_var","Grouping Variable",NULL,width="200px")),
                           div(numericInput("mu","Hypothesis Mean (\u03bc\u2080)",40,width="160px"))),
                       div(class="g-2col",
                           div(class="card",div(class="card-header","One-Sample Tests"),
                               div(style="padding:16px;",uiOutput("one_sample_ui"))),
                           div(class="card",div(class="card-header","Two-Sample Tests"),
                               div(style="padding:16px;",uiOutput("two_sample_ui"))))
      ),
      
      # ── REGRESSION ────────────────────────────────────────
      conditionalPanel("input.main_nav === 'regression'",
                       div(class="page-header",
                           div(class="page-title","Regression"),
                           div(class="page-subtitle","Simple, multiple, and indicator-variable regression")),
                       div(class="analysis-tabs",
                           tabsetPanel(id="reg_tabs", type="tabs",
                                       
                                       # SLR
                                       tabPanel("Simple Linear Regression", br(),
                                                div(class="controls-bar",
                                                    div(selectInput("slr_x","X (Predictor)",NULL,width="180px")),
                                                    div(selectInput("slr_y","Y (Response)", NULL,width="180px")),
                                                    div(textInput("slr_x0","New X values (comma-sep)",
                                                                  placeholder="e.g. 20,30",width="200px")),
                                                    div(numericInput("slr_level","Conf. Level",0.95,0.01,0.99,0.01,width="120px")),
                                                    div(style="align-self:flex-end;",
                                                        actionButton("load_tab_topic2","↓  Topic 2 Sample",
                                                                     style="background:#f0fdf4;border:1px solid #bbf7d0;color:#166534;font-size:12px;font-weight:600;border-radius:8px;padding:8px 14px;cursor:pointer;white-space:nowrap;"))),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Scatter + Fitted Line + CI Band"),
                                                        div(style="padding:12px;",plotOutput("slr_scatter",height=280))),
                                                    div(class="card",div(class="card-header","Residuals vs Fitted"),
                                                        div(style="padding:12px;",plotOutput("slr_resid",height=280)))),
                                                div(class="section-divider","Model Output"),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Model Summary"),
                                                        div(style="padding:16px;",uiOutput("slr_summary_ui"))),
                                                    div(class="card",div(class="card-header","ANOVA Table"),
                                                        div(style="padding:16px;",uiOutput("slr_anova_ui")))),
                                                div(class="section-divider","Inference"),
                                                div(class="g-3col",
                                                    div(class="card",div(class="card-header","Confidence Intervals"),
                                                        div(style="padding:16px;",uiOutput("slr_confint_ui"))),
                                                    div(class="card",div(class="card-header","Prediction Interval"),
                                                        div(style="padding:16px;",uiOutput("slr_pred_ui"))),
                                                    div(class="card",div(class="card-header","Confidence Interval"),
                                                        div(style="padding:16px;",uiOutput("slr_ci_ui")))),
                                                div(class="section-divider","Correlation"),
                                                div(class="card",div(class="card-header","Correlation Tests"),
                                                    div(style="padding:16px;",uiOutput("slr_cor_ui")))
                                       ),
                                       
                                       # MLR
                                       tabPanel("Multiple Linear Regression", br(),
                                                div(class="controls-bar",
                                                    div(selectInput("mlr_y","Y (Response)",  NULL,width="160px")),
                                                    div(selectInput("mlr_x1","X1 (Predictor)",NULL,width="160px")),
                                                    div(selectInput("mlr_x2","X2 (Predictor)",NULL,width="160px")),
                                                    div(textInput("mlr_x10","New X1 (comma-sep)",
                                                                  placeholder="e.g. 25,10",width="170px")),
                                                    div(textInput("mlr_x20","New X2 (comma-sep)",
                                                                  placeholder="e.g. 1200,1100",width="170px")),
                                                    div(numericInput("mlr_level","Conf. Level",0.95,0.01,0.99,0.01,width="120px")),
                                                    div(style="align-self:flex-end;display:flex;gap:8px;",
                                                        actionButton("load_tab_topic3","↓  Topic 3 Sample",
                                                                     style="background:#f0fdf4;border:1px solid #bbf7d0;color:#166534;font-size:12px;font-weight:600;border-radius:8px;padding:8px 14px;cursor:pointer;white-space:nowrap;"),
                                                        actionButton("load_tab_topic4","↓  Topic 4 Sample",
                                                                     style="background:#eff6ff;border:1px solid #bfdbfe;color:#1e40af;font-size:12px;font-weight:600;border-radius:8px;padding:8px 14px;cursor:pointer;white-space:nowrap;"))),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Scatter Matrix"),
                                                        div(style="padding:12px;",plotOutput("mlr_pairs",height=320))),
                                                    div(class="card",div(class="card-header","Correlation Matrix"),
                                                        div(style="padding:12px;",plotOutput("mlr_corrplot",height=320)))),
                                                div(class="section-divider","Model Output"),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Model Summary"),
                                                        div(style="padding:16px;",uiOutput("mlr_summary_ui"))),
                                                    div(class="card",div(class="card-header","Overall ANOVA"),
                                                        div(style="padding:16px;",uiOutput("mlr_anova_ui")))),
                                                div(class="section-divider","Inference & Diagnostics"),
                                                div(class="g-3col",
                                                    div(class="card",div(class="card-header","Coeff. CIs"),
                                                        div(style="padding:16px;",uiOutput("mlr_confint_ui"))),
                                                    div(class="card",div(class="card-header","Partial F-Tests"),
                                                        div(style="padding:16px;",uiOutput("mlr_partial_f_ui"))),
                                                    div(class="card",div(class="card-header","VIF"),
                                                        div(style="padding:16px;",uiOutput("mlr_vif_ui")))),
                                                div(class="section-divider","Residual Diagnostics"),
                                                div(class="g-3col",
                                                    div(class="card",div(class="card-header","Residuals vs Fitted"),
                                                        div(style="padding:12px;",plotOutput("mlr_resid",height=220))),
                                                    div(class="card",div(class="card-header","QQ Plot"),
                                                        div(style="padding:12px;",plotOutput("mlr_qqplot",height=220))),
                                                    div(class="card",div(class="card-header","Cook's Distance"),
                                                        div(style="padding:12px;",plotOutput("mlr_cooks",height=220)))),
                                                div(class="g-2col", style="margin-top:16px;",
                                                    div(class="card",div(class="card-header","Leverage"),
                                                        div(style="padding:12px;",plotOutput("mlr_leverage",height=220))),
                                                    div(class="card",div(class="card-header","Residuals Over Time"),
                                                        div(style="padding:12px;",plotOutput("mlr_resid_time_raw",height=220)))),
                                                div(class="section-divider","Statistical Tests"),
                                                div(class="g-3col",
                                                    div(class="card",div(class="card-header","Shapiro-Wilk"),
                                                        div(style="padding:16px;",uiOutput("mlr_shapiro_ui"))),
                                                    div(class="card",div(class="card-header","Breusch-Pagan"),
                                                        div(style="padding:16px;",uiOutput("mlr_bp_ui"))),
                                                    div(class="card",div(class="card-header","Durbin-Watson"),
                                                        div(style="padding:16px;",uiOutput("mlr_dw_ui")))),
                                                div(class="section-divider","Prediction for New Data"),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Prediction & Confidence Intervals"),
                                                        div(style="padding:16px;",uiOutput("mlr_pred_ui"))),
                                                    div(class="card",div(class="card-header","Extrapolation Check + X1 vs X2"),
                                                        div(style="padding:16px;",uiOutput("mlr_extrap_ui")),
                                                        div(style="padding:12px;",plotOutput("mlr_x1x2_plot",height=220))))
                                       ),
                                       
                                       # IDR
                                       tabPanel("Indicator / Dummy Regression", br(),
                                                div(class="controls-bar",
                                                    div(selectInput("idr_y","Y (Response)",          NULL,width="180px")),
                                                    div(selectInput("idr_x","X (Numeric Predictor)", NULL,width="180px")),
                                                    div(selectInput("idr_g","Group (Categorical)",   NULL,width="180px")),
                                                    div(textInput("idr_x0","New X values (comma-sep)",
                                                                  placeholder="e.g. 700,850",width="180px")),
                                                    div(textInput("idr_g0","New Group values (comma-sep)",
                                                                  placeholder="e.g. A,B",width="180px")),
                                                    div(numericInput("idr_level","Conf. Level",0.95,0.01,0.99,0.01,width="120px")),
                                                    div(style="align-self:flex-end;",
                                                        actionButton("load_tab_topic8","↓  Topic 8 Sample",
                                                                     style="background:#f0fdf4;border:1px solid #bbf7d0;color:#166534;font-size:12px;font-weight:600;border-radius:8px;padding:8px 14px;cursor:pointer;white-space:nowrap;"))),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Parallel Lines (Model 2)"),
                                                        div(style="padding:12px;",plotOutput("idr_plot_m2",height=300))),
                                                    div(class="card",div(class="card-header","Different Slopes (Model 3)"),
                                                        div(style="padding:12px;",plotOutput("idr_plot_m3",height=300)))),
                                                div(class="section-divider","Model Output"),
                                                div(class="g-1col",
                                                    div(class="card",div(class="card-header","Model 1 \u2014 Common Line"),
                                                        div(style="padding:16px;",uiOutput("idr_sum1_ui"))),
                                                    div(class="card",div(class="card-header","Model 2 \u2014 Same Slope, Diff. Intercepts"),
                                                        div(style="padding:16px;",uiOutput("idr_sum2_ui"))),
                                                    div(class="card",div(class="card-header","Model 3 \u2014 Diff. Slopes + Intercepts"),
                                                        div(style="padding:16px;",uiOutput("idr_sum3_ui")))),
                                                div(class="section-divider","Confidence Intervals"),
                                                div(class="g-1col",
                                                    div(class="card",div(class="card-header","Model 1 CIs"),
                                                        div(style="padding:16px;",uiOutput("idr_ci1_ui"))),
                                                    div(class="card",div(class="card-header","Model 2 CIs"),
                                                        div(style="padding:16px;",uiOutput("idr_ci2_ui"))),
                                                    div(class="card",div(class="card-header","Model 3 CIs"),
                                                        div(style="padding:16px;",uiOutput("idr_ci3_ui")))),
                                                div(class="section-divider","Nested Model Comparisons"),
                                                div(class="card",div(class="card-header","ANOVA Comparisons"),
                                                    div(style="padding:16px;",uiOutput("idr_anova_ui"))),
                                                div(class="section-divider","Prediction for New Data"),
                                                div(class="g-1col",
                                                    div(class="card",div(class="card-header","Model 1"),
                                                        div(style="padding:16px;",uiOutput("idr_pred1_ui"))),
                                                    div(class="card",div(class="card-header","Model 2"),
                                                        div(style="padding:16px;",uiOutput("idr_pred2_ui"))),
                                                    div(class="card",div(class="card-header","Model 3"),
                                                        div(style="padding:16px;",uiOutput("idr_pred3_ui"))))
                                       )
                           ))
      ),# ── TRANSFORMATIONS ───────────────────────────────────
      conditionalPanel("input.main_nav === 'transforms'",
                       div(class="page-header",
                           div(class="page-title","Transformations"),
                           div(class="page-subtitle","Box-Cox, Box-Tidwell, and Weighted Least Squares")),
                       div(class="analysis-tabs",
                           tabsetPanel(id="trans_tabs", type="tabs",
                                       
                                       tabPanel("Box-Cox", br(),
                                                div(class="controls-bar",
                                                    div(selectInput("bc_x","X (Predictor)",NULL,width="180px")),
                                                    div(selectInput("bc_y","Y (Response)", NULL,width="180px")),
                                                    div(numericInput("bc_lam_min","\u03bb Min",  -0.5, step=0.1, width="110px")),
                                                    div(numericInput("bc_lam_max","\u03bb Max",   1.5, step=0.1, width="110px")),
                                                    div(numericInput("bc_lam_step","Step",0.001,min=0.0001,max=0.1,step=0.001,width="110px")),
                                                    div(style="align-self:flex-end;",
                                                        div(class="card",
                                                            style="padding:10px 16px;background:#f0fdf4 !important;border-color:#bbf7d0 !important;",
                                                            uiOutput("bc_lambda_inline"))),
                                                    div(style="align-self:flex-end;",
                                                        actionButton("load_tab_topic5","\u2193  Topic 5 Sample",
                                                                     style="background:#f0fdf4;border:1px solid #bbf7d0;color:#166534;font-size:12px;font-weight:600;border-radius:8px;padding:8px 14px;cursor:pointer;white-space:nowrap;"))),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Original Scatter"),
                                                        div(style="padding:12px;",plotOutput("bc_scatter_orig",height=260))),
                                                    div(class="card",div(class="card-header","Box-Cox Log-Likelihood"),
                                                        div(style="padding:12px;",plotOutput("bc_plot",height=260)))),
                                                div(class="section-divider","Residual Comparison"),
                                                div(class="g-3col",
                                                    div(class="card",div(class="card-header","Original (model1)"),
                                                        div(style="padding:12px;",plotOutput("bc_resid1",height=220))),
                                                    div(class="card",div(class="card-header","\u221ay Guess (model2)"),
                                                        div(style="padding:12px;",plotOutput("bc_resid2",height=220))),
                                                    div(class="card",div(class="card-header","Box-Cox Optimal (model3)"),
                                                        div(style="padding:12px;",plotOutput("bc_resid3",height=220)))),
                                                div(class="section-divider","Scatter Comparison"),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","\u221ay Transformed Scatter"),
                                                        div(style="padding:12px;",plotOutput("bc_scatter2",height=240))),
                                                    div(class="card",div(class="card-header","Box-Cox Transformed Scatter"),
                                                        div(style="padding:12px;",plotOutput("bc_scatter3",height=240)))),
                                                div(class="section-divider","Model Summaries"),
                                                div(class="g-1col",
                                                    div(class="card",div(class="card-header","model1 \u2014 Original"),
                                                        div(style="padding:20px;",uiOutput("bc_sum1_ui"))),
                                                    div(class="card",div(class="card-header","model2 \u2014 \u221ay"),
                                                        div(style="padding:20px;",uiOutput("bc_sum2_ui"))),
                                                    div(class="card",div(class="card-header","model3 \u2014 Box-Cox"),
                                                        div(style="padding:20px;",uiOutput("bc_sum3_ui"))))
                                       ),
                                       
                                       tabPanel("Box-Tidwell", br(),
                                                div(class="controls-bar",
                                                    div(selectInput("bt_x","X (Predictor)",NULL,width="180px")),
                                                    div(selectInput("bt_y","Y (Response)", NULL,width="180px")),
                                                    div(style="align-self:flex-end;",
                                                        div(class="card",
                                                            style="padding:10px 16px;background:#f0fdf4 !important;border-color:#bbf7d0 !important;",
                                                            uiOutput("bt_alpha_inline"))),
                                                    div(style="align-self:flex-end;",
                                                        actionButton("load_tab_topic5_bt","↓  Topic 5 Sample",
                                                                     style="background:#f0fdf4;border:1px solid #bbf7d0;color:#166534;font-size:12px;font-weight:600;border-radius:8px;padding:8px 14px;cursor:pointer;white-space:nowrap;"))),
                                                div(class="card",div(class="card-header","Original Scatter"),
                                                    div(style="padding:12px;",plotOutput("bt_scatter_orig",height=280))),
                                                div(class="section-divider","Residual Comparison"),
                                                div(class="g-3col",
                                                    div(class="card",div(class="card-header","Original (model1)"),
                                                        div(style="padding:12px;",plotOutput("bt_resid1",height=220))),
                                                    div(class="card",div(class="card-header","1/x Guess (model2)"),
                                                        div(style="padding:12px;",plotOutput("bt_resid2",height=220))),
                                                    div(class="card",div(class="card-header","Box-Tidwell (model3)"),
                                                        div(style="padding:12px;",plotOutput("bt_resid3",height=220)))),
                                                div(class="section-divider","Fitted Comparison"),
                                                div(class="g-3col",
                                                    div(class="card",div(class="card-header","Original Fit"),
                                                        div(style="padding:12px;",plotOutput("bt_fit1",height=220))),
                                                    div(class="card",div(class="card-header","1/x Fit"),
                                                        div(style="padding:12px;",plotOutput("bt_fit2",height=220))),
                                                    div(class="card",div(class="card-header","Box-Tidwell Fit"),
                                                        div(style="padding:12px;",plotOutput("bt_fit3",height=220)))),
                                                div(class="section-divider","Model Summaries"),
                                                div(class="g-1col",
                                                    div(class="card",div(class="card-header","model1"),
                                                        div(style="padding:20px;",uiOutput("bt_sum1_ui"))),
                                                    div(class="card",div(class="card-header","model2 \u2014 1/x"),
                                                        div(style="padding:20px;",uiOutput("bt_sum2_ui"))),
                                                    div(class="card",div(class="card-header","model3 \u2014 Box-Tidwell"),
                                                        div(style="padding:20px;",uiOutput("bt_sum3_ui"))))
                                       ),
                                       
                                       tabPanel("Weighted Least Squares", br(),
                                                div(class="controls-bar",
                                                    div(selectInput("wls_x","X (Predictor)",  NULL,width="180px")),
                                                    div(selectInput("wls_y","Y (Response)",   NULL,width="180px")),
                                                    div(selectInput("wls_var","Variance Column",NULL,width="200px")),
                                                    div(style="align-self:flex-end;",
                                                        actionButton("load_tab_topic5_wls","↓  Topic 5 Sample",
                                                                     style="background:#f0fdf4;border:1px solid #bbf7d0;color:#166534;font-size:12px;font-weight:600;border-radius:8px;padding:8px 14px;cursor:pointer;white-space:nowrap;"))),
                                                div(class="card",div(class="card-header","Scatter \u2014 Original Data"),
                                                    div(style="padding:12px;",plotOutput("wls_scatter",height=280))),
                                                div(class="section-divider","Residual Comparison"),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","OLS Residuals"),
                                                        div(style="padding:12px;",plotOutput("wls_resid_ols",height=260))),
                                                    div(class="card",div(class="card-header","WLS Weighted Residuals"),
                                                        div(style="padding:12px;",plotOutput("wls_resid_wls",height=260)))),
                                                div(class="section-divider","Model Summaries"),
                                                div(class="g-1col",
                                                    div(class="card",div(class="card-header","OLS Summary"),
                                                        div(style="padding:20px;",uiOutput("wls_sum_ols_ui"))),
                                                    div(class="card",div(class="card-header","WLS Summary (weights = 1/var)"),
                                                        div(style="padding:20px;",uiOutput("wls_sum_wls_ui"))))
                                       )
                           ))
      ),
      
      # ── INFLUENCE & ROBUST ────────────────────────────────
      conditionalPanel("input.main_nav === 'influence'",
                       div(class="page-header",
                           div(class="page-title","Influence Diagnostics & Robust Regression"),
                           div(class="page-subtitle","Topic 6 \u2014 Identify influential observations, compare deletion vs. robust down-weighting.")),
                       div(class="controls-bar",
                           div(selectInput("inf_y","Y (Response)",NULL,width="150px")),
                           div(selectInput("inf_x","Predictors (X)",NULL,multiple=TRUE,width="420px")),
                           div(style="align-self:flex-end;",
                               actionButton("load_tab_topic6","\u2193  Topic 6 Sample",
                                            style="background:#fef2f2;border:1px solid #fecaca;color:#991b1b;font-size:12px;font-weight:600;border-radius:8px;padding:8px 14px;cursor:pointer;white-space:nowrap;")),
                           div(style="align-self:flex-end;",
                               actionButton("run_inf","Run Analysis",
                                            style="background:#22c55e;color:white;border:none;border-radius:8px;padding:8px 14px;font-weight:600;"))),
                       div(class="section-divider","Step 1 \u2014 Full Data OLS  (Model 1)"),
                       div(class="g-2col",
                           div(class="card",div(class="card-header","Model 1 \u2014 Coefficients & Fit"),
                               div(style="padding:16px;",uiOutput("inf_m1_summary_ui"))),
                           div(class="card",div(class="card-header","Residuals vs Fitted"),
                               div(style="padding:12px;",plotOutput("inf_m1_resid",height=320)))),
                       div(class="section-divider","Step 2 \u2014 Influence Diagnostics"),
                       div(class="g-3col",
                           div(class="card",div(class="card-header","Cook's Distance"),
                               div(style="padding:12px;",plotOutput("inf_cooks",height=240))),
                           div(class="card",div(class="card-header","Leverage h\u1d62\u1d62"),
                               div(style="padding:12px;",plotOutput("inf_leverage",height=240))),
                           div(class="card",div(class="card-header","DFFITS"),
                               div(style="padding:12px;",plotOutput("inf_dffits",height=240)))),
                       div(class="card", style="margin-top:16px;",
                           div(class="card-header","Flagged Influential Observations (influence.measures)"),
                           div(style="padding:12px;",DTOutput("inf_table"))),
                       div(class="section-divider","Step 3 \u2014 OLS After Removing Influential Points  (Model 2)"),
                       div(class="g-2col",
                           div(class="card",div(class="card-header","Model 2 \u2014 Coefficients & Fit"),
                               div(style="padding:16px;",uiOutput("inf_m2_summary_ui"))),
                           div(class="card",div(class="card-header","Model 2 Residuals vs Fitted"),
                               div(style="padding:12px;",plotOutput("inf_m2_resid",height=320)))),
                       div(class="section-divider","Step 4 \u2014 Huber Robust Regression  (Model 3)"),
                       div(class="g-2col",
                           div(class="card",div(class="card-header","Huber rlm \u2014 Coefficients"),
                               div(style="padding:16px;",uiOutput("inf_m3_summary_ui"))),
                           div(class="card",div(class="card-header","Huber Residuals vs Weights"),
                               div(style="padding:12px;",plotOutput("inf_huber_weights",height=320)))),
                       div(class="card", style="margin-top:16px;",
                           div(class="card-header","10 Most Down-weighted Observations (Huber)"),
                           div(style="padding:12px;",DTOutput("inf_huber_lowest"))),
                       div(class="section-divider","Step 5 \u2014 Bisquare (Tukey) Robust Regression  (Model 4)"),
                       div(class="g-2col",
                           div(class="card",div(class="card-header","Bisquare rlm \u2014 Coefficients"),
                               div(style="padding:16px;",uiOutput("inf_m4_summary_ui"))),
                           div(class="card",div(class="card-header","Bisquare Residuals vs Weights"),
                               div(style="padding:12px;",plotOutput("inf_bisq_weights",height=320)))),
                       div(class="card", style="margin-top:16px;",
                           div(class="card-header","10 Most Down-weighted Observations (Bisquare)"),
                           div(style="padding:12px;",DTOutput("inf_bisq_lowest"))),
                       div(class="section-divider","Step 6 \u2014 Side-by-Side Model Comparison"),
                       div(class="card",
                           div(class="card-header","All Four Models \u2014 Coefficients & Scale"),
                           div(style="padding:12px;",DTOutput("inf_compare"))),
                       div(class="card", style="margin-top:16px;",
                           div(class="card-header","Coefficient Comparison Plot"),
                           div(style="padding:12px;",plotOutput("inf_compare_plot",height=320)))
      ),
      
      # ── FEATURE SELECTION ─────────────────────────────────
      conditionalPanel("input.main_nav === 'feature_selection'",
                       div(class="page-header",
                           div(class="page-title","Feature Selection"),
                           div(class="page-subtitle","Full model, best subset, Cp, AIC/BIC, and stepwise selection")),
                       div(class="controls-bar",
                           div(selectInput("fs_y","Y Response",NULL,width="180px")),
                           div(selectInput("fs_x","Predictors",NULL,multiple=TRUE,width="420px")),
                           div(style="align-self:flex-end;",
                               actionButton("run_fs","Run Feature Selection",
                                            style="background:#22c55e;color:white;border:none;border-radius:8px;padding:8px 14px;font-weight:600;"))),
                       div(class="g-2col",
                           div(class="card",div(class="card-header","Full Model Summary"),
                               div(style="padding:16px;",verbatimTextOutput("fs_full_summary"))),
                           div(class="card",div(class="card-header","ANOVA"),
                               div(style="padding:16px;",verbatimTextOutput("fs_anova")))),
                       div(class="section-divider","Correlation & Multicollinearity"),
                       div(class="g-2col",
                           div(class="card",div(class="card-header","Correlation Plot"),
                               div(style="padding:12px;",plotOutput("fs_corrplot",height=320))),
                           div(class="card",div(class="card-header","VIF Plot"),
                               div(style="padding:12px;",plotOutput("fs_vifplot",height=320)))),
                       div(class="section-divider","Best Subset Approach"),
                       div(class="g-2col",
                           div(class="card",div(class="card-header","All Possible Regressions"),
                               div(style="padding:12px;",DTOutput("fs_all_reg"))),
                           div(class="card",div(class="card-header","Best Models by Criterion"),
                               div(style="padding:16px;",verbatimTextOutput("fs_best_models")))),
                       div(class="card", style="margin-top:16px;",
                           div(class="card-header","Mallow's Cp Plot"),
                           div(style="padding:12px;",plotOutput("fs_cp_plot",height=320))),
                       div(class="section-divider","Stepwise Approaches"),
                       div(class="g-3col",
                           div(class="card",div(class="card-header","Forward Selection"),
                               div(style="padding:16px;",verbatimTextOutput("fs_forward"))),
                           div(class="card",div(class="card-header","Backward Selection"),
                               div(style="padding:16px;",verbatimTextOutput("fs_backward"))),
                           div(class="card",div(class="card-header","Stepwise Selection"),
                               div(style="padding:16px;",verbatimTextOutput("fs_stepwise")))),
                       div(class="section-divider","Final Model"),
                       div(class="card",div(class="card-header","Selected Final Model: y ~ x1 + x2 + x4"),
                           div(style="padding:16px;",verbatimTextOutput("fs_final_model")))
      ),
      
      # ── MULTICOLLINEARITY ─────────────────────────────────
      conditionalPanel("input.main_nav === 'multicol'",
                       div(class="page-header",
                           div(class="page-title","Multicollinearity"),
                           div(class="page-subtitle","Quadratic response surface, VIFs, Ridge, LASSO, and CV-RMSE comparison")),
                       div(class="controls-bar",
                           div(selectInput("mc_y", "Y (Response)",     NULL,width="160px")),
                           div(selectInput("mc_x1","X\u2081 (Predictor)",NULL,width="160px")),
                           div(selectInput("mc_x2","X\u2082 (Predictor)",NULL,width="160px")),
                           div(selectInput("mc_x3","X\u2083 (Predictor)",NULL,width="160px")),
                           div(numericInput("mc_seed","CV Seed",523132,width="120px")),
                           div(numericInput("mc_folds","CV Folds",10,min=3,max=20,width="100px")),
                           div(style="align-self:flex-end;",
                               actionButton("load_tab_topic9","\u2193  Topic 9 Sample",
                                            style="background:#f0fdf4;border:1px solid #bbf7d0;color:#166534;font-size:12px;font-weight:600;border-radius:8px;padding:8px 14px;cursor:pointer;white-space:nowrap;"))),
                       div(class="section-divider","Step 1 \u2014 Full Quadratic Surface (Raw Data)"),
                       div(class="g-2col",
                           div(class="card",div(class="card-header","Raw OLS \u2014 Coefficients & Fit"),
                               div(style="padding:16px;",uiOutput("mc_raw_summary_ui"))),
                           div(class="card",div(class="card-header","Raw VIFs (red = 5, orange = 10)"),
                               div(style="padding:12px;",plotOutput("mc_vif_raw_plot",height=280)),
                               div(style="padding:0 16px 16px;",uiOutput("mc_vif_raw_ui")))),
                       div(class="section-divider","Step 2 \u2014 Centered & Unit-Length Scaled"),
                       div(class="g-2col",
                           div(class="card",div(class="card-header","Centered OLS \u2014 Coefficients & Fit"),
                               div(style="padding:16px;",uiOutput("mc_cent_summary_ui"))),
                           div(class="card",div(class="card-header","Centered VIFs"),
                               div(style="padding:12px;",plotOutput("mc_vif_cent_plot",height=280)),
                               div(style="padding:0 16px 16px;",uiOutput("mc_vif_cent_ui")))),
                       div(class="section-divider","Step 3 \u2014 Ridge Regression (10-fold CV, \u03b1 = 0)"),
                       div(class="g-2col",
                           div(class="card",div(class="card-header","CV Curve \u2014 log(\u03bb) vs MSE"),
                               div(style="padding:12px;",plotOutput("mc_ridge_cv_plot",height=280))),
                           div(class="card",div(class="card-header","Ridge Coefficients @ \u03bb.min"),
                               div(style="padding:16px;",uiOutput("mc_ridge_coef_ui")))),
                       div(class="section-divider","Step 4 \u2014 LASSO Regression (10-fold CV, \u03b1 = 1)"),
                       div(class="g-2col",
                           div(class="card",div(class="card-header","CV Curve \u2014 log(\u03bb) vs MSE"),
                               div(style="padding:12px;",plotOutput("mc_lasso_cv_plot",height=280))),
                           div(class="card",div(class="card-header","LASSO Coefficients @ \u03bb.min"),
                               div(style="padding:16px;",uiOutput("mc_lasso_coef_ui")))),
                       div(class="section-divider","Step 5 \u2014 Predictions for New Operating Points"),
                       div(class="controls-bar",
                           div(textInput("mc_new_x1","New X\u2081 (comma-sep)",
                                         placeholder="e.g. 1300,1200",width="180px")),
                           div(textInput("mc_new_x2","New X\u2082 (comma-sep)",
                                         placeholder="e.g. 6.5,14.0", width="180px")),
                           div(textInput("mc_new_x3","New X\u2083 (comma-sep)",
                                         placeholder="e.g. 0.02,0.03",width="180px"))),
                       div(class="card",
                           div(class="card-header","Ridge & LASSO Predictions"),
                           div(style="padding:16px;",uiOutput("mc_predictions_ui"))),
                       div(class="section-divider","Step 6 \u2014 Comparative 10-fold CV-RMSE"),
                       div(class="g-2col",
                           div(class="card",div(class="card-header","RMSE Comparison Bar Plot"),
                               div(style="padding:12px;",plotOutput("mc_compare_plot",height=320))),
                           div(class="card",div(class="card-header","RMSE Values & Best Model"),
                               div(style="padding:16px;",uiOutput("mc_compare_ui"))))
      ),# ══════════════════════════════════════════════════════
      # ── TOPIC 7 — POLYNOMIAL REGRESSION & SPLINES ────────
      # ══════════════════════════════════════════════════════
      conditionalPanel("input.main_nav === 'topic7'",
                       div(class="page-header",
                           div(class="page-title","Polynomial Regression & Splines"),
                           div(class="page-subtitle","Topic 7 \u2014 Centering to fix multicollinearity in polynomial models; piecewise and B-spline regression")),
                       div(class="analysis-tabs",
                           tabsetPanel(id="t7_tabs", type="tabs",
                                       
                                       # ── HARDWOOD TAB ─────────────────────────────
                                       tabPanel("Hardwood \u2014 Polynomial & Centering", br(),
                                                div(class="controls-bar",
                                                    div(selectInput("hw_x","X (Predictor)",NULL,width="160px")),
                                                    div(selectInput("hw_y","Y (Response)", NULL,width="160px")),
                                                    div(style="align-self:flex-end;",
                                                        actionButton("load_tab_topic7_hw","\u2193  Topic 7 Hardwood",
                                                                     style="background:#f0fdf4;border:1px solid #bbf7d0;color:#166534;font-size:12px;font-weight:600;border-radius:8px;padding:8px 14px;cursor:pointer;white-space:nowrap;"))),
                                                
                                                div(class="section-divider","Step 1 \u2014 Original Scatter"),
                                                div(class="card",div(class="card-header","Original Scatter Plot"),
                                                    div(style="padding:12px;",plotOutput("hw_scatter",height=280))),
                                                
                                                div(class="section-divider","Step 2 \u2014 Linear Model (Model 1)"),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Scatter + Linear Fit"),
                                                        div(style="padding:12px;",plotOutput("hw_m1_scatter",height=260))),
                                                    div(class="card",div(class="card-header","Residuals vs Fitted \u2014 Lack of Fit"),
                                                        div(style="padding:12px;",plotOutput("hw_m1_resid",height=260)))),
                                                div(class="card",div(class="card-header","Model 1 Summary"),
                                                    div(style="padding:16px;",uiOutput("hw_m1_ui"))),
                                                
                                                div(class="section-divider","Step 3 \u2014 Quadratic Model (Model 2) \u2014 Multicollinearity Problem"),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Scatter + Quadratic Fit"),
                                                        div(style="padding:12px;",plotOutput("hw_m2_scatter",height=260))),
                                                    div(class="card",div(class="card-header","Residuals vs Fitted"),
                                                        div(style="padding:12px;",plotOutput("hw_m2_resid",height=260)))),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Model 2 Summary"),
                                                        div(style="padding:16px;",uiOutput("hw_m2_ui"))),
                                                    div(class="card",div(class="card-header","VIF \u2014 Quadratic (VIF \u2248 17, Problem!)"),
                                                        div(style="padding:12px;",plotOutput("hw_m2_vif",height=220)))),
                                                
                                                div(class="section-divider","Step 4 \u2014 Quadratic After Centering (Model 3) \u2014 Problem Solved"),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Scatter + Quadratic Fit (Centered xc)"),
                                                        div(style="padding:12px;",plotOutput("hw_m3_scatter",height=260))),
                                                    div(class="card",div(class="card-header","Residuals vs Fitted"),
                                                        div(style="padding:12px;",plotOutput("hw_m3_resid",height=260)))),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Model 3 Summary"),
                                                        div(style="padding:16px;",uiOutput("hw_m3_ui"))),
                                                    div(class="card",div(class="card-header","VIF \u2014 Quadratic Centered (VIF \u2248 1.1, Fixed!)"),
                                                        div(style="padding:12px;",plotOutput("hw_m3_vif",height=220)))),
                                                
                                                div(class="section-divider","Step 5 \u2014 Cubic Model (Model 4) \u2014 Severe Multicollinearity"),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Scatter + Cubic Fit"),
                                                        div(style="padding:12px;",plotOutput("hw_m4_scatter",height=260))),
                                                    div(class="card",div(class="card-header","Residuals vs Fitted \u2014 Improved Fit"),
                                                        div(style="padding:12px;",plotOutput("hw_m4_resid",height=260)))),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Model 4 Summary"),
                                                        div(style="padding:16px;",uiOutput("hw_m4_ui"))),
                                                    div(class="card",div(class="card-header","VIF \u2014 Cubic (Extreme Multicollinearity!)"),
                                                        div(style="padding:12px;",plotOutput("hw_m4_vif",height=220)))),
                                                
                                                div(class="section-divider","Step 6 \u2014 Cubic After Centering (Model 5) \u2014 Reduced Multicollinearity"),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Scatter + Cubic Fit (Centered xc)"),
                                                        div(style="padding:12px;",plotOutput("hw_m5_scatter",height=260))),
                                                    div(class="card",div(class="card-header","Residuals vs Fitted"),
                                                        div(style="padding:12px;",plotOutput("hw_m5_resid",height=260)))),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Model 5 Summary"),
                                                        div(style="padding:16px;",uiOutput("hw_m5_ui"))),
                                                    div(class="card",div(class="card-header","VIF \u2014 Cubic Centered (Greatly Reduced)"),
                                                        div(style="padding:12px;",plotOutput("hw_m5_vif",height=220)))),
                                                
                                                div(class="section-divider","Model Comparison"),
                                                div(class="card",div(class="card-header","All 5 Models \u2014 R\u00b2, Adj. R\u00b2, RSE"),
                                                    div(style="padding:16px;",uiOutput("hw_compare_ui")))
                                       ),
                                       
                                       # ── VOLTAGE TAB ──────────────────────────────
                                       tabPanel("Voltage Drop \u2014 Polynomial & Splines", br(),
                                                div(class="controls-bar",
                                                    div(selectInput("vt_x","X (Time)",        NULL,width="150px")),
                                                    div(selectInput("vt_y","Y (Voltage Drop)",NULL,width="180px")),
                                                    div(numericInput("vt_k2","Knot 1 (k2)", 6.5, step=0.5,width="120px")),
                                                    div(numericInput("vt_k3","Knot 2 (k3)",13.0, step=0.5,width="120px")),
                                                    div(style="align-self:flex-end;",
                                                        actionButton("load_tab_topic7_vt","\u2193  Topic 7 Voltage",
                                                                     style="background:#fff7ed;border:1px solid #fed7aa;color:#9a3412;font-size:12px;font-weight:600;border-radius:8px;padding:8px 14px;cursor:pointer;white-space:nowrap;"))),
                                                
                                                div(class="section-divider","Step 1 \u2014 Original Scatter (Piecewise Behavior)"),
                                                div(class="card",div(class="card-header","Voltage Drop vs Time \u2014 Suggests Piecewise Model"),
                                                    div(style="padding:12px;",plotOutput("vt_scatter",height=280))),
                                                
                                                div(class="section-divider","Step 2 \u2014 Polynomial Models (1 through 4)"),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Model 1 \u2014 Linear Fit (Poor)"),
                                                        div(style="padding:12px;",plotOutput("vt_m1_scatter",height=240))),
                                                    div(class="card",div(class="card-header","Model 1 Residuals"),
                                                        div(style="padding:12px;",plotOutput("vt_m1_resid",height=240)))),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Model 2 \u2014 Quadratic Fit (Poor)"),
                                                        div(style="padding:12px;",plotOutput("vt_m2_scatter",height=240))),
                                                    div(class="card",div(class="card-header","Model 2 Residuals"),
                                                        div(style="padding:12px;",plotOutput("vt_m2_resid",height=240)))),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Model 3 \u2014 Cubic Fit (Poor)"),
                                                        div(style="padding:12px;",plotOutput("vt_m3_scatter",height=240))),
                                                    div(class="card",div(class="card-header","Model 3 Residuals"),
                                                        div(style="padding:12px;",plotOutput("vt_m3_resid",height=240)))),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Model 4 \u2014 Quartic Fit \u2713 Good Poly Fit"),
                                                        div(style="padding:12px;",plotOutput("vt_m4_scatter",height=240))),
                                                    div(class="card",div(class="card-header","Model 4 Residuals"),
                                                        div(style="padding:12px;",plotOutput("vt_m4_resid",height=240)))),
                                                
                                                div(class="section-divider","Polynomial Model Summaries"),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Model 1 \u2014 Linear"),
                                                        div(style="padding:16px;",uiOutput("vt_m1_ui"))),
                                                    div(class="card",div(class="card-header","Model 2 \u2014 Quadratic"),
                                                        div(style="padding:16px;",uiOutput("vt_m2_ui")))),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Model 3 \u2014 Cubic"),
                                                        div(style="padding:16px;",uiOutput("vt_m3_ui"))),
                                                    div(class="card",div(class="card-header","Model 4 \u2014 Quartic"),
                                                        div(style="padding:16px;",uiOutput("vt_m4_ui")))),
                                                
                                                div(class="section-divider","Step 3 \u2014 Spline Models (5 through 8)"),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Model 5 \u2014 Linear Spline, 1 Knot at x=10"),
                                                        div(style="padding:12px;",plotOutput("vt_m5_scatter",height=240))),
                                                    div(class="card",div(class="card-header","Model 5 Residuals"),
                                                        div(style="padding:12px;",plotOutput("vt_m5_resid",height=240)))),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Model 6 \u2014 Linear Spline, 2 Knots (k2, k3)"),
                                                        div(style="padding:12px;",plotOutput("vt_m6_scatter",height=240))),
                                                    div(class="card",div(class="card-header","Model 6 Residuals"),
                                                        div(style="padding:12px;",plotOutput("vt_m6_resid",height=240)))),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Model 7 \u2014 Quadratic Spline, 2 Knots"),
                                                        div(style="padding:12px;",plotOutput("vt_m7_scatter",height=240))),
                                                    div(class="card",div(class="card-header","Model 7 Residuals"),
                                                        div(style="padding:12px;",plotOutput("vt_m7_resid",height=240)))),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Model 8 \u2014 Cubic Spline, 2 Knots \u2713 Best Overall"),
                                                        div(style="padding:12px;",plotOutput("vt_m8_scatter",height=240))),
                                                    div(class="card",div(class="card-header","Model 8 Residuals"),
                                                        div(style="padding:12px;",plotOutput("vt_m8_resid",height=240)))),
                                                
                                                div(class="section-divider","Spline Model Summaries"),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Model 5 \u2014 Linear Spline (1 knot)"),
                                                        div(style="padding:16px;",uiOutput("vt_m5_ui"))),
                                                    div(class="card",div(class="card-header","Model 6 \u2014 Linear Spline (2 knots)"),
                                                        div(style="padding:16px;",uiOutput("vt_m6_ui")))),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","Model 7 \u2014 Quadratic Spline (2 knots)"),
                                                        div(style="padding:16px;",uiOutput("vt_m7_ui"))),
                                                    div(class="card",div(class="card-header","Model 8 \u2014 Cubic Spline (2 knots)"),
                                                        div(style="padding:16px;",uiOutput("vt_m8_ui")))),
                                                
                                                div(class="section-divider","All 8 Models Comparison"),
                                                div(class="g-2col",
                                                    div(class="card",div(class="card-header","R\u00b2 Comparison Bar Plot"),
                                                        div(style="padding:12px;",plotOutput("vt_compare_plot",height=320))),
                                                    div(class="card",div(class="card-header","R\u00b2, Adj. R\u00b2, RSE \u2014 All 8 Models"),
                                                        div(style="padding:16px;",uiOutput("vt_compare_ui"))))
                                       )
                           ))
      ),
      
      # ── GLM ───────────────────────────────────────────────
      conditionalPanel("input.main_nav === 'bayesian'",
                       div(class="page-header",
                           div(class="page-title","Bayesian Modeling"),
                           div(class="page-subtitle","Classical OLS and Bayesian linear regression for rocket propellant data")),
                       div(class="controls-bar",
                           div(style="display:flex;align-items:center;gap:10px;flex-wrap:wrap;",
                               span(style="font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:0.08em;color:#9ca3af;white-space:nowrap;",
                                    "Quick Load:"),
                               actionButton("load_tab_topic12_bayes","\u2193  Rocket Propellant",
                                            style="background:#ede9fe;color:#5b21b6;border:1px solid #ddd6fe;border-radius:8px;padding:6px 12px;font-size:12px;font-weight:600;"))),
                       div(class="controls-bar",
                           div(selectInput("bayes_x","Predictor (X)",NULL,width="220px")),
                           div(selectInput("bayes_y","Response (Y)",NULL,width="240px")),
                           div(numericInput("bayes_prob","Interval Probability",0.95,0.50,0.99,0.01,width="150px")),
                           div(numericInput("bayes_iter","Posterior Draws",4000,1000,12000,500,width="140px")),
                           div(numericInput("bayes_seed","Seed",123,1,999999,1,width="120px")),
                           div(style="align-self:flex-end;",
                               actionButton("run_bayes","Fit Models",
                                            style="background:#22c55e;color:white;border:none;border-radius:8px;padding:8px 14px;font-weight:600;"))),
                       div(class="section-divider","Rocket Propellant Dataset"),
                       div(class="g-2col",
                           div(class="card",div(class="card-header","Data Preview"),
                               div(style="padding:0;",DTOutput("bayes_preview"))),
                           div(class="card",div(class="card-header","Scatter + OLS Fitted Line"),
                               div(style="padding:12px;",plotOutput("bayes_scatter",height=300)))),
                       div(class="section-divider","Classical vs Bayesian Output"),
                       div(class="g-2col",
                           div(class="card",div(class="card-header","Classical OLS"),
                               div(style="padding:16px;",uiOutput("bayes_ols_ui"))),
                           div(class="card",div(class="card-header","Bayesian Posterior Summary"),
                               div(style="padding:16px;",uiOutput("bayes_post_ui")))),
                       div(class="section-divider","Posterior Probabilities"),
                       div(class="g-2col",
                           div(class="card",div(class="card-header","Posterior Probability Checks"),
                               div(style="padding:16px;",uiOutput("bayes_prob_ui"))),
                           div(class="card",div(class="card-header","Posterior Draws for Slope"),
                               div(style="padding:12px;",plotOutput("bayes_slope_plot",height=280))))
      ),
      
      conditionalPanel("input.main_nav === 'glm'",
                       div(class="page-header",
                           div(class="page-title","Generalized Linear Models"),
                           div(class="page-subtitle","Logistic (logit), Probit, and Poisson regression for binary and count responses")),
                       div(class="controls-bar",
                           div(style="display:flex;align-items:center;gap:10px;flex-wrap:wrap;",
                               span(style="font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:0.08em;color:#9ca3af;white-space:nowrap;",
                                    "Quick Load:"),
                               actionButton("load_tab_topic11_binary","↓  Wellness \u2014 Logit / Probit",
                                            style="background:#dbeafe;color:#1e40af;border:1px solid #bfdbfe;border-radius:8px;padding:6px 12px;font-size:12px;font-weight:600;"),
                               actionButton("load_tab_topic11_poisson","↓  Awards \u2014 Poisson",
                                            style="background:#fee2e2;color:#991b1b;border:1px solid #fecaca;border-radius:8px;padding:6px 12px;font-size:12px;font-weight:600;"))),
                       div(class="controls-bar",
                           div(selectInput("glm_type","Response Type",
                                           choices=c("Binary \u2014 Logit / Probit"="binary",
                                                     "Count \u2014 Poisson"="poisson"),
                                           width="240px")),
                           div(selectInput("glm_y","Response (Y)",NULL,width="170px")),
                           div(selectInput("glm_x","Predictor (X)",NULL,width="170px")),
                           div(selectInput("glm_grp","Group Variable (Poisson only)",
                                           choices=c("None"=""),width="230px")),
                           div(style="align-self:flex-end;",
                               actionButton("run_glm","Fit Models",
                                            style="background:#22c55e;color:white;border:none;border-radius:8px;padding:8px 14px;font-weight:600;"))),
                       conditionalPanel("input.glm_type === 'binary'",
                                        div(class="section-divider","Model Comparison \u2014 OLS vs Logit vs Probit"),
                                        div(class="card",
                                            div(class="card-header","2 \u00d7 2 Comparison: OLS / Truncated OLS / Logit / Probit"),
                                            div(style="padding:12px;",plotOutput("glm_binary_plot",height=520))),
                                        div(class="section-divider","Model Summaries"),
                                        div(class="g-2col",
                                            div(class="card",div(class="card-header","Logit Model"),
                                                div(style="padding:16px;",uiOutput("glm_logit_ui"))),
                                            div(class="card",div(class="card-header","Probit Model"),
                                                div(style="padding:16px;",uiOutput("glm_probit_ui")))),
                                        div(class="section-divider","Predictions"),
                                        div(class="controls-bar",
                                            div(textInput("glm_binary_new_x","New X values (comma-separated)",
                                                          placeholder="e.g. 3, 5, 7",width="260px"))),
                                        div(class="card",div(class="card-header","Predicted Probabilities"),
                                            div(style="padding:16px;",uiOutput("glm_binary_pred_ui")))
                       ),
                       conditionalPanel("input.glm_type === 'poisson'",
                                        div(class="section-divider","Model Comparison \u2014 OLS vs Poisson"),
                                        div(class="card",div(class="card-header","OLS vs Poisson Fit"),
                                            div(style="padding:12px;",plotOutput("glm_pois_plot",height=320))),
                                        div(class="section-divider","Poisson Model Summary"),
                                        div(class="g-2col",
                                            div(class="card",div(class="card-header","Poisson Regression"),
                                                div(style="padding:16px;",uiOutput("glm_pois_full_ui"))),
                                            div(class="card",div(class="card-header","Likelihood Ratio Test \u2014 Group Effect"),
                                                div(style="padding:16px;",uiOutput("glm_pois_lrt_ui")))),
                                        div(class="section-divider","Predictions"),
                                        div(class="controls-bar",
                                            div(textInput("glm_pois_new_x","New X values (comma-separated)",
                                                          placeholder="e.g. 40, 50, 60",width="260px"))),
                                        div(class="card",div(class="card-header","Predicted Counts (\u03bb)"),
                                            div(style="padding:16px;",uiOutput("glm_pois_pred_ui")))
                       )
      )
      
  ) # close main-wrap
) # close page_sidebar 
############################################################
# SERVER
############################################################
server <- function(input, output, session) {
  
  data <- reactiveVal(NULL)
  
  output$sheet_ui <- renderUI({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    if (ext %in% c("xlsx","xls")) {
      sheets <- readxl::excel_sheets(input$file$datapath)
      selectInput("sheet","Sheet",choices=sheets,selected=sheets[1])
    }
  })
  
  observeEvent(list(input$file, input$sheet), {
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    df  <- if (ext=="csv") read.csv(input$file$datapath) else {
      req(input$sheet)
      as.data.frame(readxl::read_excel(input$file$datapath, sheet=input$sheet))
    }
    data(df)
    upd_all(session, df)
  }, ignoreInit=TRUE)
  
  upd_all <- function(session, df) {
    num_cols <- names(df)[sapply(df, is.numeric)]
    cat_cols <- names(df)[!sapply(df, is.numeric)]
    upd <- function(id, choices, selected=NULL)
      updateSelectInput(session, id, choices=choices,
                        selected=if(is.null(selected)) choices[1] else selected)
    
    upd("num_var", num_cols)
    upd("num_x",   num_cols)
    upd("num_y",   num_cols, num_cols[min(2,length(num_cols))])
    upd("test_var",num_cols)
    upd("slr_x",   num_cols)
    upd("slr_y",   num_cols, num_cols[min(2,length(num_cols))])
    upd("mlr_y",   num_cols, num_cols[1])
    upd("mlr_x1",  num_cols, num_cols[min(2,length(num_cols))])
    upd("mlr_x2",  num_cols, num_cols[min(3,length(num_cols))])
    upd("bc_x",    num_cols)
    upd("bc_y",    num_cols, num_cols[min(2,length(num_cols))])
    upd("bt_x",    num_cols)
    upd("bt_y",    num_cols, num_cols[min(2,length(num_cols))])
    upd("wls_x",   num_cols)
    upd("wls_y",   num_cols, num_cols[min(2,length(num_cols))])
    upd("wls_var", num_cols, num_cols[min(3,length(num_cols))])
    upd("idr_y",   num_cols, num_cols[1])
    upd("idr_x",   num_cols, num_cols[min(2,length(num_cols))])
    upd("mc_y",    num_cols, num_cols[1])
    upd("mc_x1",   num_cols, num_cols[min(2,length(num_cols))])
    upd("mc_x2",   num_cols, num_cols[min(3,length(num_cols))])
    upd("mc_x3",   num_cols, num_cols[min(4,length(num_cols))])
    upd("fs_y",    num_cols, if("y" %in% num_cols) "y" else num_cols[1])
    updateSelectInput(session, "fs_x", choices=num_cols,
                      selected=if(all(c("x1","x2","x3","x4") %in% num_cols))
                        c("x1","x2","x3","x4") else num_cols[-1])
    upd("inf_y",   num_cols, if("y" %in% num_cols) "y" else num_cols[1])
    default_inf_x <- if(all(c("x1","x2","x3","x4","x5") %in% num_cols))
      c("x1","x2","x3","x4","x5") else
        setdiff(num_cols,"y")[1:min(5,length(num_cols)-1)]
    updateSelectInput(session,"inf_x", choices=num_cols, selected=default_inf_x)
    
    # ── Topic 7 selects ──
    upd("hw_x", num_cols, if("x" %in% num_cols) "x" else num_cols[1])
    upd("hw_y", num_cols, if("y" %in% num_cols) "y" else num_cols[min(2,length(num_cols))])
    upd("vt_x", num_cols, if("x" %in% num_cols) "x" else num_cols[1])
    upd("vt_y", num_cols, if("y" %in% num_cols) "y" else num_cols[min(2,length(num_cols))])
    upd("bayes_x", num_cols, if("Age of Propellant, xi (weeks)" %in% num_cols) "Age of Propellant, xi (weeks)" else num_cols[1])
    upd("bayes_y", num_cols, if("Shear Strength, yi (psi)" %in% num_cols) "Shear Strength, yi (psi)" else num_cols[min(2,length(num_cols))])
    
    if (length(cat_cols) > 0) {
      upd("cat_var",  cat_cols)
      upd("cat_a",    cat_cols)
      upd("cat_b",    cat_cols, cat_cols[min(2,length(cat_cols))])
      upd("cat_a3",   cat_cols)
      upd("cat_b3",   cat_cols, cat_cols[min(2,length(cat_cols))])
      upd("cat_c3",   cat_cols, cat_cols[min(3,length(cat_cols))])
      upd("gender_var", cat_cols)
      upd("idr_g",    cat_cols)
    }
    all_cols <- names(df)
    upd("glm_y", all_cols, all_cols[1])
    upd("glm_x", num_cols, num_cols[min(2,length(num_cols))])
    updateSelectInput(session,"glm_grp",
                      choices=c("None"="",all_cols), selected="")
  }
  
  active_sample <- reactiveVal(NULL)
  
  load_topic_data <- function(key, navigate=FALSE) {
    ds <- sample_datasets[[key]]
    if (is.null(ds)) return()
    path <- ds$path
    if (!file.exists(path)) path <- file.path("www", ds$path)
    if (!file.exists(path)) {
      showNotification(paste("Sample file not found:", ds$path),
                       type="warning", duration=5)
      return()
    }
    ext <- tools::file_ext(path)
    df  <- tryCatch(
      if (ext %in% c("xlsx","xls")) as.data.frame(readxl::read_excel(path))
      else read.csv(path),
      error = function(e) NULL
    )
    if (is.null(df)) {
      showNotification("Could not read sample file.", type="warning", duration=5)
      return()
    }
    data(df); active_sample(key); upd_all(session, df)
    if (navigate) updateNavlistPanel(session, "main_nav", selected="overview")
  }
  
  # ── Sample dataset button observers ──
  observeEvent(input$load_dataset,      { load_topic_data(input$load_dataset, navigate=TRUE) })
  observeEvent(input$load_tab_topic2,   { load_topic_data("topic2")  })
  observeEvent(input$load_tab_topic3,   { load_topic_data("topic3")  })
  observeEvent(input$load_tab_topic4,   { load_topic_data("topic4")  })
  observeEvent(input$load_tab_topic5,   { load_topic_data("topic5")  })
  observeEvent(input$load_tab_topic5_bt,{ load_topic_data("topic5")  })
  observeEvent(input$load_tab_topic5_wls,{load_topic_data("topic5")  })
  observeEvent(input$load_tab_topic6,   { load_topic_data("topic6")  })
  observeEvent(input$load_tab_topic8,   { load_topic_data("topic8")  })
  observeEvent(input$load_tab_topic9,   { load_topic_data("topic9")  })
  observeEvent(input$load_tab_topic10,  { load_topic_data("topic10") })
  observeEvent(input$load_tab_topic12_bayes, {
    load_topic_data("topic12_bayes")
    updateNavlistPanel(session, "main_nav", selected="bayesian")
    updateSelectInput(session, "bayes_x", selected="Age of Propellant, xi (weeks)")
    updateSelectInput(session, "bayes_y", selected="Shear Strength, yi (psi)")
  })
  
  # ── Topic 7 loaders (NEW) ──
  observeEvent(input$load_tab_topic7_hw, {
    load_topic_data("topic7_hw")
    updateNavlistPanel(session, "main_nav", selected="topic7")
    updateSelectInput(session, "hw_x", selected="x")
    updateSelectInput(session, "hw_y", selected="y")
  })
  observeEvent(input$load_tab_topic7_vt, {
    load_topic_data("topic7_vt")
    updateNavlistPanel(session, "main_nav", selected="topic7")
    updateSelectInput(session, "vt_x", selected="x")
    updateSelectInput(session, "vt_y", selected="y")
  })
  
  observeEvent(input$load_tab_topic11_binary, {
    load_topic_data("topic11_binary")
    updateNavlistPanel(session,"main_nav",selected="glm")
    updateSelectInput(session,"glm_type",selected="binary")
    updateSelectInput(session,"glm_y",   selected="wellness")
    updateSelectInput(session,"glm_x",   selected="work")
    updateSelectInput(session,"glm_grp", selected="")
  })
  observeEvent(input$load_tab_topic11_poisson, {
    load_topic_data("topic11_poisson")
    updateNavlistPanel(session,"main_nav",selected="glm")
    updateSelectInput(session,"glm_type",selected="poisson")
    updateSelectInput(session,"glm_y",   selected="num_awards")
    updateSelectInput(session,"glm_x",   selected="math")
    updateSelectInput(session,"glm_grp", selected="prog")
  })
  
  # ── OVERVIEW ──────────────────────────────────────────
  output$overview_stats <- renderUI({
    df <- data()
    if (is.null(df)) return(
      div(class="empty-state",
          div(class="empty-state-icon","\U0001f4c2"),
          div(class="empty-state-title","No data loaded"),
          div(class="empty-state-text","Upload a CSV or Excel file to start analyzing your data.")))
    nc <- sum(sapply(df,is.numeric)); cc <- ncol(df)-nc; ms <- sum(is.na(df))
    div(class="g-3col", style="margin-bottom:20px;",
        div(class="stat-card",
            div(class="stat-card-label","Rows"),
            div(class="stat-card-value",nrow(df)),
            div(class="stat-card-badge","Observations")),
        div(class="stat-card",
            div(class="stat-card-label","Columns"),
            div(class="stat-card-value",ncol(df)),
            div(class="stat-card-badge",paste(nc,"numeric \u00b7",cc,"categorical"))),
        div(class="stat-card",
            div(class="stat-card-label","Missing Values"),
            div(class="stat-card-value",ms),
            div(class="stat-card-badge",if(ms==0)"Complete" else "Has missing")))
  })
  
  output$hist_controls_ui <- renderUI({
    req(data())
    nv <- names(data())[sapply(data(),is.numeric)]
    tagList(
      div(class="section-divider","Quick Histogram"),
      div(class="controls-bar",
          div(selectInput("ov_var","Variable",nv,width="180px")),
          div(sliderInput("bins","Bins",5,80,20,width="180px")),
          div(checkboxInput("freq","Frequency",TRUE))),
      div(class="card",
          div(class="card-header","Histogram with Density Overlay"),
          div(style="padding:12px;",plotOutput("ov_hist",height=240))))
  })
  
  output$ov_hist <- renderPlot({
    req(data(), input$ov_var)
    app_theme()
    x  <- na.omit(data()[[input$ov_var]])
    h  <- hist(x, freq=input$freq, breaks=input$bins,
               col="#dcfce7", border="white",
               main=paste("Distribution of",input$ov_var),
               xlab=input$ov_var,
               ylab=if(input$freq)"Frequency" else "Density")
    add_grid()
    if (!input$freq) {
      lines(density(x), col=APP_GREEN, lwd=2.5)
      xseq <- seq(min(x),max(x),length=200)
      lines(xseq, dnorm(xseq,mean(x),sd(x))*length(x)*diff(range(x))/20,
            col=APP_ORANGE, lwd=1.8, lty=2)
      legend("topright",legend=c("KDE","Normal ref."),
             col=c(APP_GREEN,APP_ORANGE),lty=c(1,2),lwd=c(2.5,1.8),
             bty="n",cex=0.78,text.col="#374151")
    } else {
      xseq <- seq(min(x),max(x),length=200)
      bw   <- diff(h$breaks)[1]
      lines(xseq, dnorm(xseq,mean(x),sd(x))*length(x)*bw,
            col=APP_ORANGE, lwd=1.8, lty=2)
      lines(density(x)$x, density(x)$y*length(x)*bw,
            col=APP_DARK, lwd=2.2)
      legend("topright",legend=c("KDE","Normal ref."),
             col=c(APP_DARK,APP_ORANGE),lty=c(1,2),lwd=c(2.2,1.8),
             bty="n",cex=0.78,text.col="#374151")
    }
    abline(v=mean(x),   col=APP_BLUE,   lwd=1.5, lty=3)
    abline(v=median(x), col=APP_PURPLE, lwd=1.5, lty=3)
    legend("topleft",
           legend=c(paste("Mean =",round(mean(x),2)),
                    paste("Median =",round(median(x),2))),
           col=c(APP_BLUE,APP_PURPLE), lty=3, lwd=1.5,
           bty="n", cex=0.75, text.col="#374151")
  })
  
  output$preview <- renderDT({
    req(data())
    datatable(data(), options=list(scrollX=TRUE,pageLength=8,dom="tip"),
              rownames=FALSE)
  })
  
  # ── SINGLE NUMERIC ────────────────────────────────────
  output$desc <- renderDT({
    req(data(), input$num_var)
    x   <- data()[[input$num_var]]
    tab <- table(x)
    datatable(
      data.frame(
        Stat  = c("Mean","Median","Mode","Min","Max","Range",
                  "Variance","SD","IQR","Skewness","Kurtosis"),
        Value = round(c(
          mean(x,na.rm=T), median(x,na.rm=T),
          as.numeric(names(tab)[which.max(tab)]),
          min(x,na.rm=T),  max(x,na.rm=T),
          diff(range(x,na.rm=T)), var(x,na.rm=T),
          sd(x,na.rm=T),   IQR(x,na.rm=T),
          moments::skewness(x,na.rm=T),
          moments::kurtosis(x,na.rm=T)), 4)),
      options=list(dom="t",pageLength=15), rownames=FALSE)
  })
  
  output$hist <- renderPlot({
    req(data(), input$num_var)
    app_theme()
    x <- na.omit(data()[[input$num_var]])
    hist(x, breaks="Sturges", freq=FALSE,
         col="#dcfce7", border="white",
         main=paste("Distribution of",input$num_var),
         xlab=input$num_var, ylab="Density")
    add_grid()
    lines(density(x), col=APP_GREEN, lwd=2.5)
    xseq <- seq(min(x),max(x),length=200)
    lines(xseq, dnorm(xseq,mean(x),sd(x)), col=APP_ORANGE, lwd=1.8, lty=2)
    abline(v=mean(x),         col=APP_BLUE, lwd=2,   lty=1)
    abline(v=mean(x)-sd(x),   col=APP_BLUE, lwd=1.2, lty=3)
    abline(v=mean(x)+sd(x),   col=APP_BLUE, lwd=1.2, lty=3)
    legend("topright",
           legend=c("KDE","Normal","Mean \u00b1 1 SD"),
           col=c(APP_GREEN,APP_ORANGE,APP_BLUE),
           lty=c(1,2,1), lwd=c(2.5,1.8,1.5),
           bty="n", cex=0.76, text.col="#374151")
    sk <- moments::skewness(x, na.rm=TRUE)
    mtext(paste0("n = ",length(x),"  |  skew = ",round(sk,3),
                 "  |  kurt = ",round(moments::kurtosis(x,na.rm=TRUE),3)),
          side=3, line=0.1, cex=0.68, col="#9ca3af")
  })
  
  output$box <- renderPlot({
    req(data(), input$num_var)
    boxplot(data()[[input$num_var]], horizontal=T,
            col="#4ade80", border="#166534",
            main=paste("Boxplot of",input$num_var))
  })
  
  # ── TWO NUMERIC ───────────────────────────────────────
  output$corr <- renderDT({
    req(data(), input$num_x, input$num_y)
    x <- data()[[input$num_x]]; y <- data()[[input$num_y]]
    datatable(
      data.frame(
        Method      = c("Pearson","Spearman"),
        Correlation = round(c(
          cor(x,y,use="complete.obs"),
          cor(x,y,method="spearman",use="complete.obs")), 6)),
      options=list(dom="t"), rownames=FALSE)
  })
  
  output$scatter <- renderPlot({
    req(data(), input$num_x, input$num_y)
    app_theme()
    x  <- data()[[input$num_x]]; y <- data()[[input$num_y]]
    cc <- complete.cases(data.frame(x,y)); x <- x[cc]; y <- y[cc]
    m  <- lm(y~x)
    xr   <- range(x); xseq <- seq(xr[1],xr[2],length=100)
    ci   <- predict(m, data.frame(x=xseq), interval="confidence")
    plot(x, y, type="n", xlab=input$num_x, ylab=input$num_y,
         main=paste(input$num_y,"vs",input$num_x))
    add_grid()
    polygon(c(xseq,rev(xseq)), c(ci[,"lwr"],rev(ci[,"upr"])),
            col=adjustcolor(APP_GREEN,0.12), border=NA)
    points(x, y, pch=21, bg=adjustcolor(APP_BLUE,0.55),
           col=APP_BLUE, cex=0.85, lwd=0.5)
    lines(xseq, ci[,"fit"], col=APP_ORANGE, lwd=2.2)
    r  <- cor(x,y,use="complete.obs"); r2 <- summary(m)$r.squared
    legend("topleft",
           legend=c(paste0("r  = ",round(r,4)),
                    paste0("R\u00b2 = ",round(r2,4))),
           bty="n", cex=0.8, text.col="#374151")
  })
  
  # ── MULTI NUMERIC ─────────────────────────────────────
  output$corr_matrix <- renderPlot({
    req(data())
    nd <- data()[sapply(data(),is.numeric)]
    validate(need(ncol(nd)>=2,"Need at least 2 numeric variables."))
    corrplot(cor(nd,use="complete.obs"), method="circle",
             col=colorRampPalette(c(APP_ORANGE,"white",APP_GREEN))(200),
             tl.cex=0.82, tl.col="#374151", cl.cex=0.72,
             addCoef.col="#374151", number.cex=0.7,
             mar=c(0,0,1.5,0))
    title("Correlation Matrix",col.main="#0f1f12",cex.main=0.92,font.main=2)
  })
  
  output$pairs_plot <- renderPlot({
    req(data())
    nd <- data()[sapply(data(),is.numeric)]
    validate(need(ncol(nd)>=2,"Need at least 2 numeric variables."))
    panel_hist <- function(x,...) {
      usr <- par("usr"); on.exit(par(usr))
      par(usr=c(usr[1:2],0,1.5))
      h <- hist(x,plot=FALSE,breaks=12)
      y <- h$counts/max(h$counts)
      rect(h$breaks[-length(h$breaks)],0,h$breaks[-1],y,
           col="#dcfce7",border="white")
    }
    panel_cor <- function(x,y,...) {
      usr <- par("usr"); on.exit(par(usr))
      par(usr=c(0,1,0,1))
      r   <- cor(x,y,use="complete.obs")
      col <- if(r>0) APP_GREEN else APP_ORANGE
      text(0.5,0.5,round(r,3),cex=1.4*abs(r)+0.6,col=col,font=2)
    }
    pairs(nd, pch=21, bg=adjustcolor(APP_BLUE,0.4), col=APP_BLUE,
          cex=0.65, main="Scatter Matrix", gap=0.5,
          diag.panel=panel_hist, upper.panel=panel_cor,
          col.axis="#6b7280", cex.axis=0.72,
          cex.labels=0.85, font.labels=2)
  }) # ── CATEGORICAL ───────────────────────────────────────
  output$freq_table <- renderDT({
    req(data(), input$cat_var)
    df_t <- as.data.frame(table(data()[[input$cat_var]], useNA="ifany"))
    names(df_t) <- c("Category","Frequency")
    datatable(df_t, options=list(dom="tip"), rownames=FALSE)
  })
  
  output$prop_table <- renderDT({
    req(data(), input$cat_var)
    df_t <- as.data.frame(prop.table(table(data()[[input$cat_var]], useNA="ifany")))
    names(df_t) <- c("Category","Proportion")
    df_t$Proportion <- round(df_t$Proportion, 4)
    datatable(df_t, options=list(dom="tip"), rownames=FALSE)
  })
  
  output$bar <- renderPlot({
    req(data(), input$cat_var)
    app_theme()
    par(mar=c(4.5,3.8,3.2,1.4))
    tbl  <- sort(table(data()[[input$cat_var]], useNA="ifany"), decreasing=TRUE)
    cols <- make_palette(length(tbl))
    bp   <- barplot(tbl, col=cols, border=NA,
                    main=paste("Frequency \u2014",input$cat_var),
                    las=2, cex.names=0.78, ylab="Count",
                    ylim=c(0,max(tbl)*1.18))
    add_grid(nx=NA)
    text(bp, tbl + max(tbl)*0.025, labels=tbl,
         cex=0.74, col="#374151", font=2)
    pct <- round(prop.table(tbl)*100, 1)
    text(bp, tbl/2, labels=paste0(pct,"%"),
         cex=0.68, col="white", font=2)
    mtext(paste0("n = ",sum(tbl)," observations"),
          side=3, line=0.1, cex=0.68, col="#9ca3af")
  })
  
  output$pie <- renderPlot({
    req(data(), input$cat_var)
    app_theme()
    par(mar=c(1.5,1,3,1))
    tbl  <- table(data()[[input$cat_var]], useNA="ifany")
    pct  <- round(prop.table(tbl)*100, 1)
    lbls <- paste0(names(tbl),"\n",pct,"%")
    cols <- make_palette(length(tbl),"Set3")
    pie(tbl, labels=lbls, col=cols, border="white",
        main=paste("Proportions \u2014",input$cat_var), cex=0.78)
    mtext(paste0("n = ",sum(tbl)), side=1, line=0.2, cex=0.68, col="#9ca3af")
  })
  
  output$pie3d <- renderPlot({
    req(data(), input$cat_var)
    app_theme()
    par(mar=c(1,1,3,1))
    tbl  <- table(data()[[input$cat_var]], useNA="ifany")
    pct  <- round(prop.table(tbl)*100, 1)
    lbls <- paste0(names(tbl),"\n",pct,"%")
    cols <- make_palette(length(tbl),"Set3")
    pie3D(tbl, labels=lbls, labelcex=0.70, col=cols,
          main=paste("3D Pie \u2014",input$cat_var),
          explode=0.08, theta=pi/5, start=pi/6)
  })
  
  output$stacked <- renderPlot({
    req(data(), input$cat_a, input$cat_b)
    app_theme()
    par(mar=c(4.5,3.8,3.2,5.5))
    cont <- table(data()[[input$cat_a]], data()[[input$cat_b]], useNA="ifany")
    cols <- make_palette(nrow(cont))
    barplot(cont, col=cols, border=NA, main="Stacked Bar",
            las=2, ylab="Count", cex.names=0.78)
    add_grid(nx=NA)
    legend("topright", inset=c(-0.18,0), legend=rownames(cont),
           fill=cols, border=NA, bty="n", cex=0.72,
           text.col="#374151", xpd=TRUE)
  })
  
  output$side <- renderPlot({
    req(data(), input$cat_a, input$cat_b)
    app_theme()
    par(mar=c(4.5,3.8,3.2,5.5))
    cont <- table(data()[[input$cat_a]], data()[[input$cat_b]], useNA="ifany")
    cols <- make_palette(nrow(cont))
    barplot(cont, beside=TRUE, col=cols, border=NA,
            main="Side-by-Side Bar", las=2, ylab="Count", cex.names=0.78)
    add_grid(nx=NA)
    legend("topright", inset=c(-0.18,0), legend=rownames(cont),
           fill=cols, border=NA, bty="n", cex=0.72,
           text.col="#374151", xpd=TRUE)
  })
  
  output$mosaic <- renderPlot({
    req(data(), input$cat_a, input$cat_b)
    app_theme()
    mosaicplot(
      table(data()[[input$cat_a]], data()[[input$cat_b]], useNA="ifany"),
      main=paste("Mosaic:",input$cat_a,"\u00d7",input$cat_b),
      color=make_palette(nlevels(factor(data()[[input$cat_b]])),"Set2"),
      border="white", las=1, cex.axis=0.74)
  })
  
  output$cont_table <- renderDT({
    req(data(), input$cat_a, input$cat_b)
    cont <- table(data()[[input$cat_a]], data()[[input$cat_b]], useNA="ifany")
    datatable(as.data.frame.matrix(cont))
  })
  
  output$joint_table <- renderDT({
    req(data(), input$cat_a, input$cat_b)
    cont <- table(data()[[input$cat_a]], data()[[input$cat_b]], useNA="ifany")
    datatable(as.data.frame.matrix(round(prop.table(cont), 4)))
  })
  
  output$multi_table <- renderDT({
    req(data(), input$cat_a3, input$cat_b3, input$cat_c3)
    datatable(ftable(data()[[input$cat_a3]],
                     data()[[input$cat_b3]],
                     data()[[input$cat_c3]]))
  })
  
  output$flat_table <- renderDT({
    req(data(), input$cat_a3, input$cat_b3, input$cat_c3)
    datatable(ftable(data()[[input$cat_a3]] ~
                       data()[[input$cat_b3]] +
                       data()[[input$cat_c3]]))
  })
  
  output$three_bar <- renderPlot({
    req(data(), input$cat_a3, input$cat_b3, input$cat_c3)
    app_theme()
    par(mar=c(4.5,3.8,3.2,6))
    iv  <- interaction(data()[[input$cat_b3]], data()[[input$cat_c3]], drop=TRUE)
    tbl <- table(iv, data()[[input$cat_a3]], useNA="ifany")
    cols <- make_palette(nrow(tbl),"Set3")
    barplot(tbl, col=cols, border=NA, main="Three-way Barplot",
            las=2, cex.names=0.76, ylab="Count", beside=FALSE)
    add_grid(nx=NA)
    legend("topright", inset=c(-0.22,0), legend=rownames(tbl),
           fill=cols, border=NA, bty="n", cex=0.68,
           text.col="#374151", xpd=TRUE)
  })
  
  # ── HYPOTHESIS TESTS ──────────────────────────────────
  output$one_sample_ui <- renderUI({
    req(data(), input$test_var)
    x   <- na.omit(data()[[input$test_var]])
    validate(need(length(x)>=3,"Need \u2265 3 observations."))
    mu0 <- input$mu
    tt  <- t.test(x, mu=mu0)
    wt  <- suppressWarnings(wilcox.test(x, mu=mu0, exact=FALSE))
    sw_html <- ""
    if (length(x) <= 5000) {
      sw     <- shapiro.test(x)
      sw_ok  <- sw$p.value >= 0.05
      sw_html <- paste0(
        '<div class="rb-section">Normality (Shapiro-Wilk)</div>',
        assumption_html("Normal distribution", sw_ok,
                        paste0("W = ",fmt_num(sw$statistic,4),
                               ", p = ",formatC(sw$p.value,4,format="e"))))
    }
    HTML(paste0(
      '<div class="result-block">',
      '<div class="rb-section">T-Test</div>',
      '<div class="rb-row"><span class="rb-key">Sample mean</span>',
      '<span class="rb-val">',fmt_num(mean(x)),'</span></div>',
      '<div class="rb-row"><span class="rb-key">t-statistic (df = ',
      round(tt$parameter,1),')</span>',
      '<span class="rb-val">',fmt_num(tt$statistic,3),'</span></div>',
      '<div class="rb-row"><span class="rb-key">95% CI</span>',
      '<span class="rb-val">[',fmt_num(tt$conf.int[1]),', ',
      fmt_num(tt$conf.int[2]),']</span></div>',
      decision_html(tt$p.value),
      sw_html,
      '<div class="rb-divider"></div>',
      '<div class="rb-section">Wilcoxon Signed-Rank (non-parametric)</div>',
      '<div class="rb-row"><span class="rb-key">V-statistic</span>',
      '<span class="rb-val">',fmt_num(wt$statistic,2),'</span></div>',
      decision_html(wt$p.value),
      '</div>'))
  })
  
  output$two_sample_ui <- renderUI({
    req(data(), input$test_var, input$gender_var)
    df  <- data()[complete.cases(data()[,c(input$test_var,input$gender_var)]),]
    grp <- droplevels(as.factor(df[[input$gender_var]]))
    levs <- levels(grp)
    validate(need(length(levs)==2,
                  "Grouping variable must have exactly 2 levels."))
    g1 <- df[[input$test_var]][grp==levs[1]]
    g2 <- df[[input$test_var]][grp==levs[2]]
    tt <- t.test(g1, g2)
    mw <- suppressWarnings(wilcox.test(g1, g2, exact=FALSE))
    HTML(paste0(
      '<div class="result-block">',
      '<div class="rb-section">Group Summary</div>',
      '<div class="rb-row"><span class="rb-key">',levs[1],' (n = ',length(g1),')</span>',
      '<span class="rb-val">x\u0304 = ',fmt_num(mean(g1)),
      '  sd = ',fmt_num(sd(g1)),'</span></div>',
      '<div class="rb-row"><span class="rb-key">',levs[2],' (n = ',length(g2),')</span>',
      '<span class="rb-val">x\u0304 = ',fmt_num(mean(g2)),
      '  sd = ',fmt_num(sd(g2)),'</span></div>',
      '<div class="rb-divider"></div>',
      '<div class="rb-section">Welch Two-Sample T-Test</div>',
      '<div class="rb-row"><span class="rb-key">Mean difference</span>',
      '<span class="rb-val">',fmt_num(mean(g1)-mean(g2)),'</span></div>',
      '<div class="rb-row"><span class="rb-key">t-statistic (df = ',
      fmt_num(tt$parameter,2),')</span>',
      '<span class="rb-val">',fmt_num(tt$statistic,3),'</span></div>',
      '<div class="rb-row"><span class="rb-key">95% CI for diff</span>',
      '<span class="rb-val">[',fmt_num(tt$conf.int[1]),', ',
      fmt_num(tt$conf.int[2]),']</span></div>',
      decision_html(tt$p.value),
      '<div class="rb-divider"></div>',
      '<div class="rb-section">Mann-Whitney U (non-parametric)</div>',
      '<div class="rb-row"><span class="rb-key">W-statistic</span>',
      '<span class="rb-val">',fmt_num(mw$statistic,2),'</span></div>',
      decision_html(mw$p.value),
      '</div>'))
  }) # ── SLR ───────────────────────────────────────────────
  slr_model <- reactive({
    req(data(), input$slr_x, input$slr_y)
    validate(need(input$slr_x != input$slr_y, "X and Y must differ."))
    df <- data()[, c(input$slr_x, input$slr_y)]
    names(df) <- c("x","y")
    lm(y ~ x, data=df[complete.cases(df),])
  })
  
  output$slr_scatter <- renderPlot({
    req(data(), input$slr_x, input$slr_y)
    app_theme()
    x  <- data()[[input$slr_x]]; y <- data()[[input$slr_y]]
    cc <- complete.cases(data.frame(x,y)); x <- x[cc]; y <- y[cc]
    m  <- slr_model(); s <- summary(m)
    xr   <- range(x); xseq <- seq(xr[1],xr[2],length=120)
    ci   <- predict(m, data.frame(x=xseq), interval="confidence", level=input$slr_level)
    pi   <- predict(m, data.frame(x=xseq), interval="prediction", level=input$slr_level)
    ylim <- range(c(y, pi[,"lwr"], pi[,"upr"]))
    plot(x, y, type="n", xlim=xr, ylim=ylim,
         xlab=input$slr_x, ylab=input$slr_y,
         main=paste(input$slr_y,"~",input$slr_x))
    add_grid()
    polygon(c(xseq,rev(xseq)), c(pi[,"lwr"],rev(pi[,"upr"])),
            col=adjustcolor(APP_ORANGE,0.08), border=NA)
    polygon(c(xseq,rev(xseq)), c(ci[,"lwr"],rev(ci[,"upr"])),
            col=adjustcolor(APP_GREEN,0.15), border=NA)
    points(x, y, pch=21, bg=adjustcolor(APP_BLUE,0.5),
           col=APP_BLUE, cex=0.85, lwd=0.5)
    lines(xseq, ci[,"fit"], col=APP_ORANGE, lwd=2.2)
    if (nchar(trimws(input$slr_x0)) > 0) {
      x0 <- suppressWarnings(as.numeric(trimws(unlist(strsplit(input$slr_x0,",")))))
      x0 <- x0[!is.na(x0)]
      if (length(x0) > 0) {
        y0 <- predict(m, data.frame(x=x0))
        points(x0, y0, pch=23, bg=APP_ORANGE, col="white", cex=1.6, lwd=1.5)
      }
    }
    r  <- cor(x,y); r2 <- s$r.squared
    legend("topleft",
           legend=c(paste0("r  = ",round(r,4)),
                    paste0("R\u00b2 = ",round(r2,4))),
           bty="n", cex=0.78, text.col="#374151")
    legend("bottomright",
           legend=c("Data","Fitted",
                    paste0(round(input$slr_level*100),"% CI"),
                    paste0(round(input$slr_level*100),"% PI")),
           col=c(APP_BLUE,APP_ORANGE,APP_GREEN,APP_ORANGE),
           pch=c(21,NA,15,15), lty=c(NA,1,NA,NA),
           pt.bg=c(adjustcolor(APP_BLUE,0.5),NA,
                   adjustcolor(APP_GREEN,0.3),
                   adjustcolor(APP_ORANGE,0.15)),
           pt.cex=c(1.1,NA,1.5,1.5), lwd=c(NA,2.2,NA,NA),
           bty="n", cex=0.74, text.col="#374151")
    mtext(paste0("n = ",length(x),"  |  \u0177 = ",
                 round(coef(m)[1],4)," + ",round(coef(m)[2],4),"x"),
          side=3, line=0.1, cex=0.68, col="#9ca3af")
  })
  
  output$slr_resid <- renderPlot({
    app_theme()
    m  <- slr_model(); fi <- fitted(m); ei <- residuals(m)
    plot(fi, ei, type="n",
         xlab="Fitted Values", ylab="Residuals",
         main="Residuals vs Fitted")
    add_grid()
    abline(h=0, col=APP_GRAY, lwd=1.5, lty=2)
    sg <- sd(ei)
    abline(h= 2*sg, col=APP_ORANGE, lwd=1.2, lty=3)
    abline(h=-2*sg, col=APP_ORANGE, lwd=1.2, lty=3)
    lo <- loess(ei~fi); xo <- sort(fi)
    lines(xo, predict(lo,xo), col=APP_RED, lwd=2)
    cols_pts <- ifelse(abs(ei)>2*sg, APP_RED, adjustcolor(APP_PURPLE,0.65))
    points(fi, ei, pch=21, bg=cols_pts,
           col=adjustcolor("#ffffff",0), cex=0.85)
    out_idx <- which(abs(ei)>2*sg)
    if (length(out_idx)>0)
      text(fi[out_idx], ei[out_idx], labels=out_idx,
           cex=0.62, pos=3, col=APP_RED)
    legend("topright",
           legend=c("Residual","\u00b12\u03c3 bound","LOESS","Outlier"),
           col=c(adjustcolor(APP_PURPLE,0.65),APP_ORANGE,APP_RED,APP_RED),
           pch=c(21,NA,NA,21), lty=c(NA,3,1,NA),
           pt.bg=c(adjustcolor(APP_PURPLE,0.65),NA,NA,APP_RED),
           lwd=c(NA,1.2,2,NA), bty="n", cex=0.72, text.col="#374151")
    mtext(paste0("Shapiro-Wilk p = ",
                 tryCatch(formatC(shapiro.test(ei)$p.value,3,format="e"),
                          error=function(e)"\u2014")),
          side=3, line=0.1, cex=0.68, col="#9ca3af")
  })
  
  output$slr_summary_ui <- renderUI({
    m <- slr_model(); s <- summary(m)
    HTML(model_summary_html(m, s, c(input$slr_x), input$slr_y,
                            c(input$slr_x),
                            subtitle=paste("lm(",input$slr_y,"~",input$slr_x,")")))
  })
  
  output$slr_anova_ui <- renderUI({
    m <- slr_model(); atab <- anova(m)
    HTML(paste0('<div class="result-block">',
                anova_table_html(atab, c(input$slr_x,"Residuals")),
                '</div>'))
  })
  
  output$slr_confint_ui <- renderUI({
    m   <- slr_model()
    ci  <- confint(m, level=input$slr_level)
    pct <- round(input$slr_level*100)
    HTML(paste0('<div class="result-block">',
                ci_table_html(ci, c("(Intercept)",input$slr_x), pct),
                '</div>'))
  })
  
  parse_x0 <- function(txt) {
    v <- suppressWarnings(as.numeric(trimws(unlist(strsplit(txt,",")))))
    v[!is.na(v)]
  }
  
  output$slr_pred_ui <- renderUI({
    req(input$slr_x0)
    x0  <- parse_x0(input$slr_x0)
    validate(need(length(x0)>0,"Enter valid numeric X values."))
    xr  <- range(data()[[input$slr_x]], na.rm=TRUE)
    pct <- round(input$slr_level*100)
    nd  <- data.frame(x=x0)
    out <- predict(slr_model(), nd, interval="prediction", level=input$slr_level)
    rownames(out) <- paste0("x = ",x0)
    HTML(paste0('<div class="result-block">',
                '<div class="rb-section">Prediction Interval (',pct,'%)</div>',
                pred_table_html(out, xr), '</div>'))
  })
  
  output$slr_ci_ui <- renderUI({
    req(input$slr_x0)
    x0  <- parse_x0(input$slr_x0)
    validate(need(length(x0)>0,"Enter valid numeric X values."))
    xr  <- range(data()[[input$slr_x]], na.rm=TRUE)
    pct <- round(input$slr_level*100)
    nd  <- data.frame(x=x0)
    out <- predict(slr_model(), nd, interval="confidence", level=input$slr_level)
    rownames(out) <- paste0("x = ",x0)
    HTML(paste0('<div class="result-block">',
                '<div class="rb-section">Confidence Interval (',pct,'%)</div>',
                pred_table_html(out, xr), '</div>'))
  })
  
  output$slr_cor_ui <- renderUI({
    req(data(), input$slr_x, input$slr_y)
    x <- data()[[input$slr_x]]; y <- data()[[input$slr_y]]
    rows <- ""
    for (meth in c("pearson","spearman","kendall")) {
      ct  <- cor.test(y, x, method=meth)
      r   <- as.numeric(ct$estimate); p <- ct$p.value
      sc  <- sig_class(p)
      bar_pct <- round(abs(r)*100)
      bar_cls <- if(r>=0) "corr-pos" else "corr-neg"
      rows <- paste0(rows,
                     '<div class="rb-corr-row">',
                     '<span class="rb-corr-method">',tools::toTitleCase(meth),'</span>',
                     '<div class="rb-corr-bar-wrap">',
                     '<div class="rb-corr-bar ',bar_cls,'" style="width:',bar_pct,'%;"></div>',
                     '</div>',
                     '<span class="rb-corr-val">',fmt_num(r,4),'</span>',
                     '<span class="sig-badge ',sc,'" style="margin-left:8px;">',
                     sig_label(p),'</span>',
                     '</div>')
    }
    HTML(paste0('<div class="result-block">',rows,'</div>'))
  })
  
  # ── MLR ───────────────────────────────────────────────
  mlr_model <- reactive({
    req(data(), input$mlr_y, input$mlr_x1, input$mlr_x2)
    validate(
      need(input$mlr_y  != input$mlr_x1, "Y and X1 must differ."),
      need(input$mlr_y  != input$mlr_x2, "Y and X2 must differ."),
      need(input$mlr_x1 != input$mlr_x2, "X1 and X2 must differ."))
    df <- data()[,c(input$mlr_y, input$mlr_x1, input$mlr_x2)]
    names(df) <- c("y","x1","x2")
    lm(y ~ x1 + x2, data=df[complete.cases(df),])
  })
  
  parse_nums  <- function(txt) {
    v <- suppressWarnings(as.numeric(trimws(unlist(strsplit(txt,",")))))
    v[!is.na(v)]
  }
  parse_chars <- function(txt) {
    v <- trimws(unlist(strsplit(txt,",")))
    v[nzchar(v)]
  }
  
  output$mlr_pairs <- renderPlot({
    req(data(), input$mlr_y, input$mlr_x1, input$mlr_x2)
    app_theme()
    nd <- data()[,c(input$mlr_y, input$mlr_x1, input$mlr_x2)]
    panel_hist <- function(x,...) {
      usr <- par("usr"); on.exit(par(usr))
      par(usr=c(usr[1:2],0,1.5))
      h <- hist(x,plot=FALSE,breaks=10)
      y <- h$counts/max(h$counts)
      rect(h$breaks[-length(h$breaks)],0,h$breaks[-1],y,
           col="#dcfce7",border="white")
    }
    panel_cor <- function(x,y,...) {
      usr <- par("usr"); on.exit(par(usr))
      par(usr=c(0,1,0,1))
      r   <- cor(x,y,use="complete.obs")
      col <- if(r>0) APP_GREEN else APP_ORANGE
      text(0.5,0.5,round(r,3),cex=1.3*abs(r)+0.7,col=col,font=2)
    }
    pairs(nd, pch=21, bg=adjustcolor(APP_BLUE,0.4), col=APP_BLUE,
          cex=0.68, gap=0.5, main="Scatter Matrix",
          diag.panel=panel_hist, upper.panel=panel_cor,
          cex.labels=0.88, font.labels=2)
  })
  
  output$mlr_corrplot <- renderPlot({
    req(data(), input$mlr_y, input$mlr_x1, input$mlr_x2)
    cm <- cor(data()[,c(input$mlr_y,input$mlr_x1,input$mlr_x2)],
              use="complete.obs")
    corrplot::corrplot.mixed(cm,
                             lower="number", upper="circle",
                             tl.cex=0.82, tl.col="#374151", number.cex=0.88,
                             lower.col=colorRampPalette(c(APP_ORANGE,"#e5ede8",APP_GREEN))(100),
                             upper.col=colorRampPalette(c(APP_ORANGE,"#e5ede8",APP_GREEN))(100))
    title("Correlation Matrix",col.main="#0f1f12",cex.main=0.92,font.main=2)
  })
  
  output$mlr_summary_ui <- renderUI({
    m <- mlr_model(); s <- summary(m)
    HTML(model_summary_html(m, s,
                            c(input$mlr_x1,input$mlr_x2),
                            input$mlr_y,
                            c(input$mlr_x1,input$mlr_x2),
                            subtitle=paste("lm(",input$mlr_y,"~",
                                           input$mlr_x1,"+",input$mlr_x2,")")))
  })
  
  output$mlr_anova_ui <- renderUI({
    m <- mlr_model(); atab <- anova(m)
    HTML(paste0('<div class="result-block">',
                anova_table_html(atab,
                                 c(input$mlr_x1,input$mlr_x2,"Residuals")),
                '</div>'))
  })
  
  output$mlr_confint_ui <- renderUI({
    m   <- mlr_model()
    ci  <- confint(m, level=input$mlr_level)
    pct <- round(input$mlr_level*100)
    HTML(paste0('<div class="result-block">',
                ci_table_html(ci,
                              c("(Intercept)",input$mlr_x1,input$mlr_x2),
                              pct),
                '</div>'))
  })
  
  output$mlr_partial_f_ui <- renderUI({
    req(data(), input$mlr_y, input$mlr_x1, input$mlr_x2)
    df <- data()[,c(input$mlr_y,input$mlr_x1,input$mlr_x2)]
    names(df) <- c("y","x1","x2")
    df   <- df[complete.cases(df),]
    full <- lm(y~x1+x2, data=df)
    tests <- list(
      list(label=paste0(input$mlr_x1," | ",input$mlr_x2), red=lm(y~x2,data=df)),
      list(label=paste0(input$mlr_x2," | ",input$mlr_x1), red=lm(y~x1,data=df)),
      list(label="Both (overall)",                         red=lm(y~1, data=df)))
    rows <- ""
    for (tst in tests) {
      av <- anova(tst$red, full)
      fv <- av[2,"F"]; pv <- av[2,"Pr(>F)"]; sc <- sig_class(pv)
      rows <- paste0(rows,
                     '<div class="rb-row">',
                     '<span class="rb-key">H\u2080: drop ',tst$label,'</span>',
                     '<span style="margin-right:8px;font-family:DM Mono,monospace;',
                     'font-size:12px;color:#6b7280;">F = ',fmt_num(fv,3),'</span>',
                     '<span class="sig-badge ',sc,'">',sig_label(pv),'</span>',
                     '</div>')
    }
    HTML(paste0('<div class="result-block">',rows,'</div>'))
  })
  
  output$mlr_vif_ui <- renderUI({
    m        <- mlr_model()
    vif_vals <- car::vif(m)
    names(vif_vals) <- c(input$mlr_x1, input$mlr_x2)
    max_vif  <- max(vif_vals)
    rows <- ""
    for (nm in names(vif_vals)) {
      v   <- vif_vals[nm]
      pct <- min(round(v/max(11,max_vif*1.1)*100), 100)
      cls <- if(v<5) "vif-ok" else if(v<10) "vif-warn" else "vif-high"
      rows <- paste0(rows,
                     '<div class="rb-vif-row">',
                     '<span class="rb-vif-name">',nm,'</span>',
                     '<div class="rb-vif-bar-wrap">',
                     '<div class="rb-vif-bar ',cls,'" style="width:',pct,'%;"></div>',
                     '</div>',
                     '<span class="rb-vif-val">',fmt_num(v,2),'</span>',
                     '</div>')
    }
    note <- if(max_vif<5)
      '<div style="font-size:11px;color:#6b7280;margin-top:8px;">All VIF &lt; 5 \u2014 no multicollinearity concern</div>'
    else if(max_vif<10)
      '<div style="font-size:11px;color:#92400e;margin-top:8px;">VIF 5\u201310 \u2014 moderate multicollinearity</div>'
    else
      '<div style="font-size:11px;color:#b91c1c;margin-top:8px;">VIF &gt; 10 \u2014 high multicollinearity detected</div>'
    HTML(paste0('<div class="result-block">',rows,note,'</div>'))
  })
  
  output$mlr_resid <- renderPlot({
    app_theme()
    m  <- mlr_model(); fi <- fitted(m); ei <- residuals(m); sg <- sd(ei)
    plot(fi, ei, type="n",
         xlab="Fitted Values", ylab="Residuals", main="Residuals vs Fitted")
    add_grid()
    abline(h=0, col=APP_GRAY, lwd=1.5, lty=2)
    abline(h= 2*sg, col=APP_ORANGE, lwd=1, lty=3)
    abline(h=-2*sg, col=APP_ORANGE, lwd=1, lty=3)
    lo <- loess(ei~fi); xo <- sort(fi)
    lines(xo, predict(lo,xo), col=APP_RED, lwd=2)
    col_pts <- ifelse(abs(ei)>2*sg, APP_RED, adjustcolor(APP_PURPLE,0.65))
    points(fi, ei, pch=21, bg=col_pts,
           col=adjustcolor("#ffffff",0), cex=0.82)
    out_idx <- which(abs(ei)>2*sg)
    if (length(out_idx)>0)
      text(fi[out_idx], ei[out_idx], labels=out_idx,
           cex=0.60, pos=3, col=APP_RED)
    mtext("Red = |residual| > 2\u03c3  |  Orange lines = \u00b12\u03c3",
          side=3, line=0.1, cex=0.67, col="#9ca3af")
  })
  
  output$mlr_qqplot <- renderPlot({
    app_theme()
    ei <- residuals(mlr_model())
    qqnorm(ei, pch=21, bg=adjustcolor(APP_BLUE,0.55), col=APP_BLUE, cex=0.82,
           main="Normal Q-Q Plot of Residuals",
           xlab="Theoretical Quantiles", ylab="Sample Quantiles")
    add_grid()
    qqline(ei, col=APP_ORANGE, lwd=2)
    sw <- tryCatch(shapiro.test(ei), error=function(e)NULL)
    if (!is.null(sw))
      mtext(paste0("Shapiro-Wilk: W = ",round(sw$statistic,4),
                   "  p = ",formatC(sw$p.value,3,format="e")),
            side=3, line=0.1, cex=0.68, col="#9ca3af")
  })
  
  output$mlr_cooks <- renderPlot({
    app_theme()
    m  <- mlr_model(); n <- length(residuals(m))
    cd <- cooks.distance(m); thr <- 4/n
    col_bars <- ifelse(cd>thr, APP_RED, APP_BLUE)
    plot(cd, type="h", col=col_bars, lwd=1.6,
         main="Cook's Distance",
         xlab="Observation Index", ylab="Cook's Distance",
         ylim=c(0,max(cd)*1.2))
    add_grid(nx=NA)
    abline(h=thr, col=APP_ORANGE, lty=2, lwd=1.8)
    inf_idx <- which(cd>thr)
    if (length(inf_idx)>0) {
      points(inf_idx, cd[inf_idx], pch=21, bg=APP_RED, col="white", cex=1.4)
      text(inf_idx, cd[inf_idx], labels=inf_idx,
           cex=0.65, pos=3, col=APP_RED, font=2)
    }
    legend("topright",
           legend=c(paste0("Threshold = 4/n = ",round(thr,4)),
                    paste0(length(inf_idx)," influential obs.")),
           col=c(APP_ORANGE,APP_RED), lty=c(2,NA), pch=c(NA,21),
           pt.bg=c(NA,APP_RED), bty="n", cex=0.74, text.col="#374151")
  })
  
  output$mlr_leverage <- renderPlot({
    app_theme()
    m   <- mlr_model(); lev <- hatvalues(m)
    p   <- length(coef(m)); n <- length(lev); thr <- 2*p/n
    col_bars <- ifelse(lev>thr, APP_ORANGE, APP_GREEN)
    plot(lev, type="h", col=col_bars, lwd=1.6,
         main="Leverage Values",
         xlab="Observation Index", ylab="Leverage h\u1d62\u1d62",
         ylim=c(0,max(lev)*1.2))
    add_grid(nx=NA)
    abline(h=thr, col=APP_RED, lty=2, lwd=1.8)
    hi_idx <- which(lev>thr)
    if (length(hi_idx)>0) {
      points(hi_idx, lev[hi_idx], pch=21, bg=APP_ORANGE, col="white", cex=1.4)
      text(hi_idx, lev[hi_idx], labels=hi_idx,
           cex=0.65, pos=3, col=APP_ORANGE, font=2)
    }
    legend("topright",
           legend=c(paste0("Threshold 2p/n = ",round(thr,4)),
                    paste0(length(hi_idx)," high-leverage")),
           col=c(APP_RED,APP_ORANGE), lty=c(2,NA), pch=c(NA,21),
           pt.bg=c(NA,APP_ORANGE), bty="n", cex=0.74, text.col="#374151")
  })
  
  output$mlr_resid_time_raw <- renderPlot({
    app_theme()
    ei <- resid(mlr_model()); n <- length(ei); sg <- sd(ei)
    col_pts <- ifelse(abs(ei)>2*sg, APP_RED, APP_BLUE)
    plot(seq_along(ei), ei, type="n",
         xlab="Observation Order", ylab="Residual",
         main="Residuals vs Order")
    add_grid()
    abline(h=0,    col=APP_GRAY,   lwd=1.5, lty=2)
    abline(h= 2*sg, col=APP_ORANGE, lwd=1,   lty=3)
    abline(h=-2*sg, col=APP_ORANGE, lwd=1,   lty=3)
    segments(seq_along(ei), 0, seq_along(ei), ei,
             col=adjustcolor(APP_BLUE,0.3), lwd=1)
    points(seq_along(ei), ei, pch=21, bg=col_pts,
           col=adjustcolor("#ffffff",0), cex=0.82)
    lo <- loess(ei~seq_along(ei))
    lines(seq_along(ei), predict(lo), col=APP_RED, lwd=2)
    mtext("LOESS trend shown in red  |  Orange lines = \u00b12\u03c3",
          side=3, line=0.1, cex=0.67, col="#9ca3af")
  })
  
  output$mlr_shapiro_ui <- renderUI({
    ei <- resid(mlr_model())
    validate(need(length(ei)>=3 && length(ei)<=5000,
                  "Sample outside 3-5000 range."))
    sw <- shapiro.test(ei); ok <- sw$p.value >= 0.05
    HTML(paste0(
      '<div class="result-block">',
      '<div class="rb-row"><span class="rb-key">W-statistic</span>',
      '<span class="rb-val">',fmt_num(sw$statistic,4),'</span></div>',
      '<div class="rb-row"><span class="rb-key">p-value</span>',
      '<span class="rb-val">',formatC(sw$p.value,4,format="e"),'</span></div>',
      '<div class="rb-divider"></div>',
      assumption_html("Normality of residuals", ok,
                      if(ok) "Residuals appear normally distributed (p \u2265 0.05)"
                      else   "Non-normal residuals detected (p < 0.05)"),
      '</div>'))
  })
  
  output$mlr_bp_ui <- renderUI({
    m  <- mlr_model(); bp <- lmtest::bptest(m); ok <- bp$p.value >= 0.05
    HTML(paste0(
      '<div class="result-block">',
      '<div class="rb-row"><span class="rb-key">BP-statistic (df = ',
      bp$parameter,')</span>',
      '<span class="rb-val">',fmt_num(bp$statistic,4),'</span></div>',
      '<div class="rb-row"><span class="rb-key">p-value</span>',
      '<span class="rb-val">',formatC(bp$p.value,4,format="e"),'</span></div>',
      '<div class="rb-divider"></div>',
      assumption_html("Homoscedasticity", ok,
                      if(ok) "Constant variance (p \u2265 0.05)"
                      else   "Non-constant variance \u2014 consider WLS or transform"),
      '</div>'))
  })
  
  output$mlr_dw_ui <- renderUI({
    m  <- mlr_model(); dw <- lmtest::dwtest(m); ok <- dw$p.value >= 0.05
    dw_range_ok <- dw$statistic >= 1.5 && dw$statistic <= 2.5
    HTML(paste0(
      '<div class="result-block">',
      '<div class="rb-row"><span class="rb-key">DW-statistic</span>',
      '<span class="rb-val">',fmt_num(dw$statistic,4),
      if(!dw_range_ok) ' <span style="font-size:11px;color:#f97316;">(outside 1.5\u20132.5)</span>'
      else             ' <span style="font-size:11px;color:#22c55e;">(1.5\u20132.5 \u2713)</span>',
      '</span></div>',
      '<div class="rb-row"><span class="rb-key">p-value</span>',
      '<span class="rb-val">',formatC(dw$p.value,4,format="e"),'</span></div>',
      '<div class="rb-divider"></div>',
      assumption_html("Independence of residuals", ok,
                      if(ok) "No autocorrelation detected (p \u2265 0.05)"
                      else   "Positive autocorrelation detected"),
      '</div>'))
  })
  
  output$mlr_pred_ui <- renderUI({
    req(input$mlr_x10, input$mlr_x20)
    x10 <- parse_nums(input$mlr_x10); x20 <- parse_nums(input$mlr_x20)
    validate(need(length(x10)>0 && length(x10)==length(x20),
                  "Equal-length lists required."))
    pct    <- round(input$mlr_level*100)
    nd     <- data.frame(x1=x10, x2=x20)
    pi_out <- predict(mlr_model(), nd, interval="prediction", level=input$mlr_level)
    ci_out <- predict(mlr_model(), nd, interval="confidence", level=input$mlr_level)
    rownames(pi_out) <- rownames(ci_out) <- paste0("(",x10,", ",x20,")")
    HTML(paste0(
      '<div class="result-block">',
      '<div class="rb-section">Prediction Interval (',pct,'%)</div>',
      pred_table_html(pi_out, NULL),
      '<div class="rb-divider"></div>',
      '<div class="rb-section">Confidence Interval (',pct,'%)</div>',
      pred_table_html(ci_out, NULL),
      '</div>'))
  })
  
  output$mlr_extrap_ui <- renderUI({
    req(input$mlr_x10, input$mlr_x20)
    x10 <- parse_nums(input$mlr_x10); x20 <- parse_nums(input$mlr_x20)
    validate(need(length(x10)>0 && length(x10)==length(x20),
                  "Equal-length lists required."))
    nd    <- data.frame(x1=x10, x2=x20)
    m     <- mlr_model(); hii <- hatvalues(m); h_max <- max(hii)
    h_new <- (predict(m, nd, interval="confidence", se.fit=TRUE)$se.fit/sigma(m))^2
    rows  <- ""
    for (i in seq_along(x10)) {
      is_ex <- h_new[i] > h_max
      tag   <- if(is_ex) '<span class="extrap-tag">Extrapolation</span>'
      else      '<span class="interp-tag">Interpolation</span>'
      rows  <- paste0(rows,
                      '<div class="rb-row">',
                      '<span class="rb-key">(',x10[i],', ',x20[i],') &nbsp;',tag,'</span>',
                      '<span class="rb-val">h = ',fmt_num(h_new[i],5),'</span>',
                      '</div>')
    }
    HTML(paste0(
      '<div class="result-block">',
      '<div class="rb-row"><span class="rb-key">Max training leverage</span>',
      '<span class="rb-val">',fmt_num(h_max,5),'</span></div>',
      '<div class="rb-divider"></div>',
      rows,'</div>'))
  })
  
  output$mlr_x1x2_plot <- renderPlot({
    req(data(), input$mlr_x1, input$mlr_x2)
    app_theme()
    x1 <- data()[[input$mlr_x1]]; x2 <- data()[[input$mlr_x2]]
    plot(x1, x2, pch=21, bg=adjustcolor(APP_BLUE,0.45), col=APP_BLUE,
         cex=0.85, lwd=0.4,
         xlab=input$mlr_x1, ylab=input$mlr_x2,
         main=paste(input$mlr_x2,"vs",input$mlr_x1))
    add_grid()
    cc <- complete.cases(data.frame(x1,x2))
    xc <- x1[cc]; yc <- x2[cc]
    ch <- tryCatch(chull(xc,yc), error=function(e)NULL)
    if (!is.null(ch))
      polygon(xc[ch], yc[ch],
              col=adjustcolor(APP_GREEN,0.06),
              border=adjustcolor(APP_GREEN,0.4), lwd=1.2, lty=2)
    if (nchar(trimws(input$mlr_x10))>0 && nchar(trimws(input$mlr_x20))>0) {
      x10 <- parse_nums(input$mlr_x10); x20 <- parse_nums(input$mlr_x20)
      if (length(x10)>0 && length(x10)==length(x20))
        points(x10, x20, pch=23, bg=APP_ORANGE, col="white", cex=1.8, lwd=1.5)
    }
    mtext(paste0("n = ",sum(complete.cases(data.frame(x1,x2))),
                 " complete observations"),
          side=3, line=0.1, cex=0.68, col="#9ca3af")
  })# ── IDR ───────────────────────────────────────────────
  idr_df <- reactive({
    req(data(), input$idr_y, input$idr_x, input$idr_g)
    validate(
      need(input$idr_y != input$idr_x, "Y and X must differ."),
      need(input$idr_y != input$idr_g, "Y and Group must differ."),
      need(input$idr_x != input$idr_g, "X and Group must differ."))
    df <- data()[,c(input$idr_y, input$idr_x, input$idr_g)]
    names(df) <- c("y","x","g")
    df <- df[complete.cases(df),]
    df$g <- as.factor(df$g)
    validate(need(length(levels(df$g))==2,
                  "Grouping variable must have exactly 2 levels."))
    df
  })
  
  idr_model1 <- reactive({ lm(y ~ x,   data=idr_df()) })
  idr_model2 <- reactive({ lm(y ~ x+g, data=idr_df()) })
  idr_model3 <- reactive({ lm(y ~ x*g, data=idr_df()) })
  
  idr_newdata <- reactive({
    req(input$idr_x0, input$idr_g0)
    x0 <- parse_nums(input$idr_x0)
    g0 <- parse_chars(input$idr_g0)
    validate(
      need(length(x0)>0, "Enter at least one valid numeric X value."),
      need(length(g0)>0, "Enter at least one group value."),
      need(length(x0)==length(g0), "X and group values must match in length."))
    g_levels <- levels(idr_df()$g)
    validate(need(all(g0 %in% g_levels),
                  paste("Group values must be one of:",
                        paste(g_levels, collapse=", "))))
    data.frame(x=x0, g=factor(g0, levels=g_levels))
  })
  
  idr_scatter_base <- function(df, m_common, m_group, levs, main_title, group_cols) {
    app_theme()
    plot(df$x[df$g==levs[1]], df$y[df$g==levs[1]],
         xlim=range(df$x), ylim=range(df$y),
         pch=21, bg=adjustcolor(group_cols[1],0.6), col=group_cols[1],
         cex=0.9, lwd=0.4,
         xlab=input$idr_x, ylab=input$idr_y, main=main_title)
    add_grid()
    points(df$x[df$g==levs[2]], df$y[df$g==levs[2]],
           pch=22, bg=adjustcolor(group_cols[2],0.6), col=group_cols[2],
           cex=0.9, lwd=0.4)
    ord <- order(df$x)
    lines(df$x[ord], fitted(m_common)[ord], col=APP_GRAY, lwd=1.6, lty=2)
    for (i in seq_along(levs)) {
      idx  <- which(df$g==levs[i])
      ordi <- order(df$x[idx])
      lines(df$x[idx][ordi], fitted(m_group)[idx][ordi],
            col=group_cols[i], lwd=2.2)
    }
    legend("topleft",
           legend=c(levs,"Common line (M1)"),
           col=c(group_cols,APP_GRAY),
           pch=c(21,22,NA), lty=c(NA,NA,2),
           pt.bg=c(adjustcolor(group_cols[1],0.6),
                   adjustcolor(group_cols[2],0.6),NA),
           lwd=c(NA,NA,1.6), bty="n", cex=0.76, text.col="#374151")
    r1 <- summary(m_common)$r.squared
    r2 <- summary(m_group)$r.squared
    mtext(paste0("M1 R\u00b2 = ",round(r1,4),
                 "  |  This model R\u00b2 = ",round(r2,4)),
          side=3, line=0.1, cex=0.68, col="#9ca3af")
  }
  
  output$idr_plot_m2 <- renderPlot({
    df <- idr_df(); levs <- levels(df$g); cols <- c(APP_BLUE,APP_GREEN)
    idr_scatter_base(df, idr_model1(), idr_model2(), levs,
                     "Model 2: Parallel Lines (same slope, diff. intercepts)", cols)
  })
  output$idr_plot_m3 <- renderPlot({
    df <- idr_df(); levs <- levels(df$g); cols <- c(APP_BLUE,APP_GREEN)
    idr_scatter_base(df, idr_model1(), idr_model3(), levs,
                     "Model 3: Interacting Lines (diff. slopes & intercepts)", cols)
  })
  
  idr_sum_ui_fn <- function(m) {
    s <- summary(m)
    HTML(paste0(
      '<div class="result-block">',
      '<div class="rb-section">Coefficients</div>',
      coef_table_html(s, rownames(s$coefficients)),
      '<div class="rb-section">Fit Statistics</div>',
      fit_stats_html(s), '</div>'))
  }
  output$idr_sum1_ui <- renderUI({ idr_sum_ui_fn(idr_model1()) })
  output$idr_sum2_ui <- renderUI({ idr_sum_ui_fn(idr_model2()) })
  output$idr_sum3_ui <- renderUI({ idr_sum_ui_fn(idr_model3()) })
  
  idr_ci_ui_fn <- function(m, pct) {
    ci <- confint(m, level=pct/100)
    HTML(paste0('<div class="result-block">',
                ci_table_html(ci, rownames(ci), pct),
                '</div>'))
  }
  output$idr_ci1_ui <- renderUI({ idr_ci_ui_fn(idr_model1(), round(input$idr_level*100)) })
  output$idr_ci2_ui <- renderUI({ idr_ci_ui_fn(idr_model2(), round(input$idr_level*100)) })
  output$idr_ci3_ui <- renderUI({ idr_ci_ui_fn(idr_model3(), round(input$idr_level*100)) })
  
  output$idr_anova_ui <- renderUI({
    comparisons <- list(
      list(m1=idr_model1(), m2=idr_model2(),
           label="M1 vs M2 \u2014 group shifts intercept?"),
      list(m1=idr_model1(), m2=idr_model3(),
           label="M1 vs M3 \u2014 slopes differ by group?"),
      list(m1=idr_model2(), m2=idr_model3(),
           label="M2 vs M3 \u2014 interaction needed?"))
    rows <- ""
    for (cp in comparisons) {
      av <- anova(cp$m1, cp$m2)
      fv <- av[2,"F"]; pv <- av[2,"Pr(>F)"]; sc <- sig_class(pv)
      rows <- paste0(rows,
                     '<div class="rb-row">',
                     '<span class="rb-key">',cp$label,'</span>',
                     '<span style="font-family:DM Mono,monospace;font-size:12px;',
                     'color:#6b7280;margin-right:8px;">F = ',fmt_num(fv,3),'</span>',
                     '<span class="sig-badge ',sc,'">',sig_label(pv),'</span>',
                     '</div>')
    }
    HTML(paste0('<div class="result-block">',rows,'</div>'))
  })
  
  idr_pred_ui_fn <- function(m, nd, level, label) {
    pct    <- round(level*100)
    ci_out <- predict(m, newdata=nd, level=level, interval="confidence")
    pi_out <- predict(m, newdata=nd, level=level, interval="prediction")
    HTML(paste0(
      '<div class="result-block">',
      '<div class="rb-section">Confidence Interval (',pct,'%)</div>',
      pred_table_html(ci_out, NULL),
      '<div class="rb-divider"></div>',
      '<div class="rb-section">Prediction Interval (',pct,'%)</div>',
      pred_table_html(pi_out, NULL),
      '</div>'))
  }
  output$idr_pred1_ui <- renderUI({
    nd <- idr_newdata()[,"x",drop=FALSE]; names(nd) <- "x"
    idr_pred_ui_fn(idr_model1(), nd, input$idr_level, "Model 1")
  })
  output$idr_pred2_ui <- renderUI({
    idr_pred_ui_fn(idr_model2(), idr_newdata(), input$idr_level, "Model 2")
  })
  output$idr_pred3_ui <- renderUI({
    idr_pred_ui_fn(idr_model3(), idr_newdata(), input$idr_level, "Model 3")
  })
  
  # ── BOX-COX ───────────────────────────────────────────
  bc_model1 <- reactive({
    req(data(), input$bc_x, input$bc_y)
    df <- data()[,c(input$bc_x,input$bc_y)]; names(df) <- c("x","y")
    lm(y~x, data=df[complete.cases(df),])
  })
  
  bc_result <- reactive({
    req(data(), input$bc_x, input$bc_y)
    df <- data()[,c(input$bc_x,input$bc_y)]; names(df) <- c("x","y")
    df <- df[complete.cases(df),]
    validate(need(all(df$y>0),"Y must be positive for Box-Cox."))
    MASS::boxcox(y~x, data=df,
                 lambda=seq(input$bc_lam_min, input$bc_lam_max,
                            by=input$bc_lam_step),
                 plotit=FALSE)
  })
  
  bc_lambda <- reactive({
    bc <- bc_result(); bc$x[which.max(bc$y)]
  })
  
  output$bc_lambda_inline <- renderUI({
    tryCatch(
      div(style="font-size:12px;color:#166534;",
          strong("Optimal \u03bb: "), round(bc_lambda(),4)),
      error=function(e) div("-"))
  })
  
  output$bc_scatter_orig <- renderPlot({
    req(data(), input$bc_x, input$bc_y)
    app_theme()
    x  <- data()[[input$bc_x]]; y <- data()[[input$bc_y]]
    cc <- complete.cases(data.frame(x,y)); x <- x[cc]; y <- y[cc]
    m  <- lm(y~x)
    plot(x, y, pch=21, bg=adjustcolor(APP_BLUE,0.5), col=APP_BLUE,
         cex=0.85, lwd=0.4,
         xlab=input$bc_x, ylab=input$bc_y,
         main=paste("Original:",input$bc_y,"vs",input$bc_x))
    add_grid(); abline(m, col=APP_ORANGE, lwd=2.2)
    mtext(paste0("R\u00b2 = ",round(summary(m)$r.squared,4)),
          side=3, line=0.1, cex=0.68, col="#9ca3af")
  })
  
  output$bc_plot <- renderPlot({
    app_theme()
    bc      <- bc_result(); lam_opt <- bc_lambda()
    max_ll  <- max(bc$y); ci_thr  <- max_ll - qchisq(0.95,1)/2
    plot(bc$x, bc$y, type="l", col=APP_PURPLE, lwd=2.4,
         xlab=expression(lambda), ylab="Log-Likelihood",
         main="Box-Cox: Log-Likelihood Profile")
    add_grid()
    abline(h=ci_thr, col=APP_GRAY,   lty=2, lwd=1.5)
    abline(v=lam_opt, col=APP_ORANGE, lty=2, lwd=2)
    ci_x <- bc$x[bc$y >= ci_thr]
    if (length(ci_x)>1)
      polygon(c(min(ci_x),min(ci_x),max(ci_x),max(ci_x)),
              c(par("usr")[3],ci_thr,ci_thr,par("usr")[3]),
              col=adjustcolor(APP_PURPLE,0.08), border=NA)
    points(lam_opt, max_ll, pch=23, bg=APP_ORANGE, col="white", cex=1.8)
    text(lam_opt, max_ll,
         labels=paste0("\u03bb = ",round(lam_opt,4)),
         pos=4, cex=0.80, col=APP_ORANGE, font=2)
    legend("bottomright",
           legend=c(paste0("Optimal \u03bb = ",round(lam_opt,4)),
                    "95% CI region"),
           col=c(APP_ORANGE,adjustcolor(APP_PURPLE,0.4)),
           lty=c(2,NA), pch=c(23,15),
           pt.bg=c(APP_ORANGE,adjustcolor(APP_PURPLE,0.2)),
           bty="n", cex=0.76, text.col="#374151")
  })
  
  resid_themed <- function(m, main_label, pt_col) {
    app_theme()
    fi <- fitted(m); ei <- residuals(m); sg <- sd(ei)
    plot(fi, ei, type="n",
         xlab="Fitted Values", ylab="Residuals", main=main_label)
    add_grid()
    abline(h=0,    col=APP_GRAY,   lwd=1.5, lty=2)
    abline(h= 2*sg, col=APP_ORANGE, lwd=1,   lty=3)
    abline(h=-2*sg, col=APP_ORANGE, lwd=1,   lty=3)
    col_pts <- ifelse(abs(ei)>2*sg, APP_RED, adjustcolor(pt_col,0.65))
    points(fi, ei, pch=21, bg=col_pts,
           col=adjustcolor("#ffffff",0), cex=0.82)
    lo <- loess(ei~fi); xo <- sort(fi)
    lines(xo, predict(lo,xo), col=APP_RED, lwd=1.8)
    bp_p <- tryCatch(lmtest::bptest(m)$p.value, error=function(e)NA)
    mtext(paste0("BP p = ",
                 if(!is.na(bp_p)) formatC(bp_p,3,format="e") else "\u2014",
                 "  |  \u00b12\u03c3 bands shown"),
          side=3, line=0.1, cex=0.67, col="#9ca3af")
  }
  
  output$bc_resid1 <- renderPlot({
    resid_themed(bc_model1(),
                 paste0("model1 \u2014 Original: ",input$bc_y," ~ ",input$bc_x),
                 APP_RED)
  })
  
  bc_model2 <- reactive({
    req(data(), input$bc_x, input$bc_y)
    df <- data()[,c(input$bc_x,input$bc_y)]; names(df) <- c("x","y")
    df <- df[complete.cases(df),]
    validate(need(all(df$y>0),"Y must be positive."))
    lm(sqrt(y)~x, data=df)
  })
  
  output$bc_resid2 <- renderPlot({
    resid_themed(bc_model2(),
                 paste0("model2 \u2014 sqrt(y): sqrt(",input$bc_y,") ~ ",input$bc_x),
                 APP_ORANGE)
  })
  
  bc_model3 <- reactive({
    req(data(), input$bc_x, input$bc_y)
    df  <- data()[,c(input$bc_x,input$bc_y)]; names(df) <- c("x","y")
    df  <- df[complete.cases(df),]
    lam <- bc_lambda()
    validate(need(all(df$y>0),"Y must be positive."))
    y_t <- if(abs(lam)<1e-8) log(df$y) else (df$y^lam-1)/lam
    lm(y_t ~ df$x)
  })
  
  output$bc_resid3 <- renderPlot({
    resid_themed(bc_model3(),
                 paste0("model3 \u2014 Box-Cox (\u03bb = ",round(bc_lambda(),4),")"),
                 APP_GREEN)
  })
  
  output$bc_scatter2 <- renderPlot({
    req(data(), input$bc_x, input$bc_y)
    df <- data()[,c(input$bc_x,input$bc_y)]; names(df) <- c("x","y")
    df <- df[complete.cases(df),]
    validate(need(all(df$y>0),"Y must be positive."))
    app_theme(); m <- bc_model2()
    plot(df$x, sqrt(df$y),
         pch=21, bg=adjustcolor(APP_BLUE,0.5), col=APP_BLUE,
         cex=0.85, lwd=0.4,
         xlab=input$bc_x, ylab=paste0("sqrt(",input$bc_y,")"),
         main="\u221ay Transformed Scatter")
    add_grid(); abline(m, col=APP_ORANGE, lwd=2.2)
    mtext(paste0("R\u00b2 = ",round(summary(m)$r.squared,4)),
          side=3, line=0.1, cex=0.68, col="#9ca3af")
  })
  
  output$bc_scatter3 <- renderPlot({
    req(data(), input$bc_x, input$bc_y)
    df  <- data()[,c(input$bc_x,input$bc_y)]; names(df) <- c("x","y")
    df  <- df[complete.cases(df),]
    lam <- bc_lambda()
    validate(need(all(df$y>0),"Y must be positive."))
    df$y_t <- if(abs(lam)<1e-8) log(df$y) else (df$y^lam-1)/lam
    app_theme(); m <- bc_model3()
    ylab <- if(abs(lam)<1e-8) paste0("log(",input$bc_y,")")
    else               paste0("BC(",input$bc_y,")")
    plot(df$x, df$y_t,
         pch=21, bg=adjustcolor(APP_BLUE,0.5), col=APP_BLUE,
         cex=0.85, lwd=0.4,
         xlab=input$bc_x, ylab=ylab,
         main=paste0("Box-Cox Transformed (\u03bb = ",round(lam,4),")"))
    add_grid(); abline(m, col=APP_ORANGE, lwd=2.2)
    mtext(paste0("R\u00b2 = ",round(summary(m)$r.squared,4),
                 "  |  \u03bb = ",round(lam,4)),
          side=3, line=0.1, cex=0.68, col="#9ca3af")
  })
  
  bc_sum_ui_fn <- function(m, label) {
    s  <- summary(m)
    bp <- tryCatch(lmtest::bptest(m), error=function(e)NULL)
    bp_html <- ""
    if (!is.null(bp)) {
      ok <- bp$p.value >= 0.05
      bp_html <- paste0(
        '<div class="rb-divider"></div>',
        assumption_html("Homoscedasticity (Breusch-Pagan)", ok,
                        paste0("BP = ",fmt_num(bp$statistic,4),
                               ", p = ",formatC(bp$p.value,3,format="e"),
                               if(ok)" \u2014 constant variance" else " \u2014 heteroscedastic")))
    }
    HTML(paste0(
      '<div class="result-block">',
      coef_table_html(s, rownames(s$coefficients)),
      '<div class="rb-section">Fit Statistics</div>',
      fit_stats_html(s), bp_html, '</div>'))
  }
  output$bc_sum1_ui <- renderUI({ bc_sum_ui_fn(bc_model1(), paste0("model1 \u2014 lm(",input$bc_y," ~ ",input$bc_x,")")) })
  output$bc_sum2_ui <- renderUI({ bc_sum_ui_fn(bc_model2(), paste0("model2 \u2014 lm(sqrt(",input$bc_y,") ~ ",input$bc_x,")")) })
  output$bc_sum3_ui <- renderUI({ bc_sum_ui_fn(bc_model3(), paste0("model3 \u2014 Box-Cox [\u03bb = ",round(bc_lambda(),4),"]")) })
  
  # ── BOX-TIDWELL ───────────────────────────────────────
  bt_model1 <- reactive({
    req(data(), input$bt_x, input$bt_y)
    df <- data()[,c(input$bt_x,input$bt_y)]; names(df) <- c("x","y")
    lm(y~x, data=df[complete.cases(df),])
  })
  
  bt_result <- reactive({
    req(data(), input$bt_x, input$bt_y)
    df <- data()[,c(input$bt_x,input$bt_y)]; names(df) <- c("x","y")
    df <- df[complete.cases(df),]
    validate(need(all(df$x>0),"X must be strictly positive for Box-Tidwell."))
    car::boxTidwell(y~x, data=df)
  })
  
  bt_alpha <- reactive({ bt_result()$result[1] })
  
  output$bt_alpha_inline <- renderUI({
    tryCatch(
      div(style="font-size:12px;color:#166634;",
          strong("Optimal \u03b1: "), round(bt_alpha(),4)),
      error=function(e) div("-"))
  })
  
  output$bt_scatter_orig <- renderPlot({
    req(data(), input$bt_x, input$bt_y)
    app_theme()
    x  <- data()[[input$bt_x]]; y <- data()[[input$bt_y]]
    cc <- complete.cases(data.frame(x,y)); x <- x[cc]; y <- y[cc]
    m  <- bt_model1()
    plot(x, y, pch=21, bg=adjustcolor(APP_BLUE,0.5), col=APP_BLUE,
         cex=0.85, lwd=0.4,
         xlab=input$bt_x, ylab=input$bt_y,
         main=paste("Original:",input$bt_y,"vs",input$bt_x))
    add_grid(); abline(m, col=APP_ORANGE, lwd=2.2)
    mtext(paste0("R\u00b2 = ",round(summary(m)$r.squared,4)),
          side=3, line=0.1, cex=0.68, col="#9ca3af")
  })
  
  output$bt_resid1 <- renderPlot({
    resid_themed(bt_model1(),
                 paste0("model1 \u2014 Original: ",input$bt_y," ~ ",input$bt_x),
                 APP_RED)
  })
  
  bt_model2 <- reactive({
    req(data(), input$bt_x, input$bt_y)
    df <- data()[,c(input$bt_x,input$bt_y)]; names(df) <- c("x","y")
    df <- df[complete.cases(df),]
    validate(need(all(df$x!=0),"X must be non-zero."))
    lm(y~I(1/x), data=df)
  })
  
  output$bt_resid2 <- renderPlot({
    resid_themed(bt_model2(),
                 paste0("model2 \u2014 1/x: ",input$bt_y," ~ 1/",input$bt_x),
                 APP_ORANGE)
  })
  
  output$bt_fit1 <- renderPlot({
    req(data(), input$bt_x, input$bt_y); app_theme()
    x  <- data()[[input$bt_x]]; y <- data()[[input$bt_y]]
    cc <- complete.cases(data.frame(x,y)); x <- x[cc]; y <- y[cc]
    m  <- bt_model1()
    plot(x, y, pch=21, bg=adjustcolor(APP_BLUE,0.5), col=APP_BLUE,
         cex=0.82, lwd=0.4,
         xlab=input$bt_x, ylab=input$bt_y, main="Original Fit")
    add_grid(); abline(m, col=APP_ORANGE, lwd=2.2)
    mtext(paste0("R\u00b2 = ",round(summary(m)$r.squared,4)),
          side=3, line=0.1, cex=0.68, col="#9ca3af")
  })
  
  output$bt_fit2 <- renderPlot({
    req(data(), input$bt_x, input$bt_y)
    df <- data()[,c(input$bt_x,input$bt_y)]; names(df) <- c("x","y")
    df <- df[complete.cases(df),]
    validate(need(all(df$x!=0),"X must be non-zero."))
    app_theme(); m <- bt_model2()
    plot(1/df$x, df$y,
         pch=21, bg=adjustcolor(APP_BLUE,0.5), col=APP_BLUE,
         cex=0.82, lwd=0.4,
         xlab=paste0("1/",input$bt_x), ylab=input$bt_y,
         main="1/x Fit")
    add_grid(); abline(m, col=APP_ORANGE, lwd=2.2)
    mtext(paste0("R\u00b2 = ",round(summary(m)$r.squared,4)),
          side=3, line=0.1, cex=0.68, col="#9ca3af")
  })
  
  bt_model3 <- reactive({
    req(data(), input$bt_x, input$bt_y)
    df    <- data()[,c(input$bt_x,input$bt_y)]; names(df) <- c("x","y")
    df    <- df[complete.cases(df),]
    alpha <- bt_alpha()
    validate(need(all(df$x>0),"X must be positive."))
    lm(y~I(x^alpha), data=df)
  })
  
  output$bt_resid3 <- renderPlot({
    resid_themed(bt_model3(),
                 paste0("model3 \u2014 Box-Tidwell (\u03b1 = ",round(bt_alpha(),4),")"),
                 APP_GREEN)
  })
  
  output$bt_fit3 <- renderPlot({
    req(data(), input$bt_x, input$bt_y)
    df    <- data()[,c(input$bt_x,input$bt_y)]; names(df) <- c("x","y")
    df    <- df[complete.cases(df),]
    alpha <- bt_alpha()
    validate(need(all(df$x>0),"X must be positive."))
    app_theme(); m <- bt_model3()
    plot(df$x^alpha, df$y,
         pch=21, bg=adjustcolor(APP_BLUE,0.5), col=APP_BLUE,
         cex=0.82, lwd=0.4,
         xlab=paste0(input$bt_x,"^",round(alpha,4)),
         ylab=input$bt_y,
         main=paste0("Box-Tidwell Fit (\u03b1 = ",round(alpha,4),")"))
    add_grid(); abline(m, col=APP_ORANGE, lwd=2.2)
    mtext(paste0("R\u00b2 = ",round(summary(m)$r.squared,4),
                 "  |  \u03b1 = ",round(alpha,4)),
          side=3, line=0.1, cex=0.68, col="#9ca3af")
  })
  
  bt_sum_ui_fn <- function(m) {
    s <- summary(m)
    HTML(paste0(
      '<div class="result-block">',
      coef_table_html(s, rownames(s$coefficients)),
      '<div class="rb-section">Fit Statistics</div>',
      fit_stats_html(s), '</div>'))
  }
  output$bt_sum1_ui <- renderUI({ bt_sum_ui_fn(bt_model1()) })
  output$bt_sum2_ui <- renderUI({ bt_sum_ui_fn(bt_model2()) })
  output$bt_sum3_ui <- renderUI({ bt_sum_ui_fn(bt_model3()) })
  
  # ── WLS ───────────────────────────────────────────────
  wls_ols <- reactive({
    req(data(), input$wls_x, input$wls_y)
    df <- data()[,c(input$wls_x,input$wls_y)]; names(df) <- c("x","y")
    lm(y~x, data=df[complete.cases(df),])
  })
  
  wls_wls <- reactive({
    req(data(), input$wls_x, input$wls_y, input$wls_var)
    df <- data()[,c(input$wls_x,input$wls_y,input$wls_var)]
    names(df) <- c("x","y","v")
    df <- df[complete.cases(df),]
    validate(need(all(df$v>0),"Variance column must be strictly positive."))
    lm(y~x, weights=1/v, data=df)
  })
  
  output$wls_scatter <- renderPlot({
    req(data(), input$wls_x, input$wls_y); app_theme()
    x  <- data()[[input$wls_x]]; y <- data()[[input$wls_y]]
    cc <- complete.cases(data.frame(x,y)); x <- x[cc]; y <- y[cc]
    m_ols <- wls_ols()
    plot(x, y, pch=21, bg=adjustcolor(APP_BLUE,0.5), col=APP_BLUE,
         cex=0.85, lwd=0.4,
         xlab=input$wls_x, ylab=input$wls_y,
         main=paste("Scatter:",input$wls_y,"vs",input$wls_x))
    add_grid(); abline(m_ols, col=APP_ORANGE, lwd=2.2)
    m_wls <- tryCatch(wls_wls(), error=function(e)NULL)
    if (!is.null(m_wls)) abline(m_wls, col=APP_GREEN, lwd=2.2)
    legend("topleft",
           legend=c("Data","OLS fit",
                    if(!is.null(m_wls))"WLS fit" else NULL),
           col=c(APP_BLUE,APP_ORANGE,
                 if(!is.null(m_wls))APP_GREEN else NULL),
           pch=c(21,NA,if(!is.null(m_wls))NA else NULL),
           lty=c(NA,1, if(!is.null(m_wls))1 else NULL),
           pt.bg=c(adjustcolor(APP_BLUE,0.5),NA,
                   if(!is.null(m_wls))NA else NULL),
           bty="n", cex=0.76, text.col="#374151")
  })
  
  output$wls_resid_ols <- renderPlot({
    resid_themed(wls_ols(),
                 paste0("OLS Residuals: ",input$wls_y," ~ ",input$wls_x),
                 APP_RED)
  })
  
  output$wls_resid_wls <- renderPlot({
    req(data(), input$wls_var); app_theme()
    m  <- wls_wls()
    v  <- na.omit(data()[[input$wls_var]])
    sw <- sqrt(1/v)
    n  <- min(length(sw), length(fitted(m))); sw <- sw[1:n]
    wfi <- sw * fitted(m)[1:n]
    wei <- sw * residuals(m)[1:n]
    sg  <- sd(wei)
    plot(wfi, wei, type="n",
         xlab=expression(sqrt(w[i])*hat(y)[i]),
         ylab=expression(sqrt(w[i])*e[i]),
         main="WLS Weighted Residuals")
    add_grid()
    abline(h=0,    col=APP_GRAY,   lwd=1.5, lty=2)
    abline(h= 2*sg, col=APP_ORANGE, lwd=1,   lty=3)
    abline(h=-2*sg, col=APP_ORANGE, lwd=1,   lty=3)
    lo <- loess(wei~wfi); xo <- sort(wfi)
    lines(xo, predict(lo,xo), col=APP_RED, lwd=1.8)
    col_pts <- ifelse(abs(wei)>2*sg, APP_RED, adjustcolor(APP_GREEN,0.65))
    points(wfi, wei, pch=21, bg=col_pts,
           col=adjustcolor("#ffffff",0), cex=0.82)
    bp_p <- tryCatch(lmtest::bptest(m)$p.value, error=function(e)NA)
    mtext(paste0("WLS BP p = ",
                 if(!is.na(bp_p)) formatC(bp_p,3,format="e") else "\u2014",
                 "  |  \u00b12\u03c3 bands"),
          side=3, line=0.1, cex=0.67, col="#9ca3af")
  })
  
  wls_sum_ui_fn <- function(m, label) {
    s  <- summary(m)
    bp <- tryCatch(lmtest::bptest(m), error=function(e)NULL)
    bp_html <- ""
    if (!is.null(bp)) {
      ok <- bp$p.value >= 0.05
      bp_html <- paste0(
        '<div class="rb-divider"></div>',
        assumption_html("Homoscedasticity (Breusch-Pagan)", ok,
                        paste0("BP = ",fmt_num(bp$statistic,4),
                               ", p = ",formatC(bp$p.value,3,format="e"),
                               if(ok)" \u2014 SATISFIED" else " \u2014 VIOLATED")))
    }
    HTML(paste0(
      '<div class="result-block">',
      coef_table_html(s, rownames(s$coefficients)),
      '<div class="rb-section">Fit Statistics</div>',
      fit_stats_html(s), bp_html, '</div>'))
  }
  output$wls_sum_ols_ui <- renderUI({
    wls_sum_ui_fn(wls_ols(), paste0("OLS: lm(",input$wls_y," ~ ",input$wls_x,")"))
  })
  output$wls_sum_wls_ui <- renderUI({
    wls_sum_ui_fn(wls_wls(),
                  paste0("WLS: lm(",input$wls_y," ~ ",input$wls_x,
                         ", weights = 1/",input$wls_var,")"))
  }) ############################################################
  # ── INFLUENCE & ROBUST (Topic 6) ────────────────────────
  ############################################################
  
  inf_data <- reactive({
    req(data(), input$inf_y, input$inf_x)
    validate(need(length(input$inf_x)>=1,"Choose at least one predictor."))
    validate(need(!(input$inf_y %in% input$inf_x),"Y cannot also be a predictor."))
    df <- as.data.frame(data()[,c(input$inf_y,input$inf_x),drop=FALSE])
    names(df)[1] <- "y"
    df <- df[complete.cases(df),,drop=FALSE]
    validate(need(nrow(df)>=length(input$inf_x)+5,
                  "Not enough complete rows for influence analysis."))
    df
  })
  
  inf_model1 <- eventReactive(input$run_inf, {
    df  <- inf_data()
    fml <- as.formula(paste("y ~",paste(input$inf_x,collapse=" + ")))
    lm(fml, data=df)
  })
  
  inf_measures <- reactive({
    req(inf_model1()); influence.measures(inf_model1())
  })
  
  inf_flagged_idx <- reactive({
    im <- inf_measures(); which(apply(im$is.inf,1,any))
  })
  
  inf_model2 <- reactive({
    req(inf_model1()); df <- inf_data(); idx <- inf_flagged_idx()
    if (length(idx)==0) return(inf_model1())
    fml <- as.formula(paste("y ~",paste(input$inf_x,collapse=" + ")))
    lm(fml, data=df[-idx,,drop=FALSE])
  })
  
  inf_model3 <- reactive({
    req(inf_model1()); df <- inf_data()
    fml <- as.formula(paste("y ~",paste(input$inf_x,collapse=" + ")))
    MASS::rlm(fml, data=df, psi=MASS::psi.huber)
  })
  
  inf_model4 <- reactive({
    req(inf_model1()); df <- inf_data()
    fml <- as.formula(paste("y ~",paste(input$inf_x,collapse=" + ")))
    MASS::rlm(fml, data=df, psi=MASS::psi.bisquare)
  })
  
  output$inf_m1_summary_ui <- renderUI({
    m <- inf_model1(); s <- summary(m)
    HTML(paste0('<div class="result-block">',
                '<div style="font-size:11.5px;color:#9ca3af;margin-bottom:10px;">',
                'lm( ',input$inf_y,' ~ ',paste(input$inf_x,collapse=" + "),' )</div>',
                '<div class="rb-section">Coefficients</div>',
                coef_table_html(s,rownames(s$coefficients)),
                '<div class="rb-section">Fit Statistics</div>',
                fit_stats_html(s),'</div>'))
  })
  
  output$inf_m1_resid <- renderPlot({
    app_theme()
    m  <- inf_model1(); fi <- fitted(m); ei <- residuals(m); sg <- sd(ei)
    plot(fi,ei,type="n",xlab="Fitted",ylab="Residual",
         main="Model 1 \u2014 Residuals vs Fitted")
    add_grid(); abline(h=0,col=APP_GRAY,lwd=1.5,lty=2)
    abline(h= 2*sg,col=APP_ORANGE,lwd=1,lty=3)
    abline(h=-2*sg,col=APP_ORANGE,lwd=1,lty=3)
    col_pts <- ifelse(abs(ei)>2*sg,APP_RED,adjustcolor(APP_BLUE,0.55))
    points(fi,ei,pch=21,bg=col_pts,col="white",cex=0.7,lwd=0.3)
    flag <- inf_flagged_idx()
    if (length(flag)>0)
      points(fi[flag],ei[flag],pch=21,bg=APP_RED,col="white",cex=1.0,lwd=0.5)
    legend("topright",
           legend=c("|res|<2\u03c3","|res|>2\u03c3","Influential (any criterion)"),
           col=c(APP_BLUE,APP_RED,APP_RED),pch=21,
           pt.bg=c(adjustcolor(APP_BLUE,0.55),APP_RED,APP_RED),
           bty="n",cex=0.74,text.col="#374151")
  })
  
  output$inf_cooks <- renderPlot({
    app_theme()
    m   <- inf_model1(); cd <- cooks.distance(m); n <- length(cd); thr <- 4/n
    cols <- ifelse(cd>thr,APP_RED,APP_BLUE)
    plot(cd,type="h",col=cols,lwd=1.2,xlab="Index",ylab="Cook's Distance",
         main="Cook's Distance",ylim=c(0,max(cd)*1.15))
    add_grid(nx=NA); abline(h=thr,col=APP_ORANGE,lty=2,lwd=1.6)
    inf_idx <- which(cd>thr)
    if (length(inf_idx)>0 && length(inf_idx)<30)
      text(inf_idx,cd[inf_idx],labels=inf_idx,cex=0.6,pos=3,col=APP_RED)
    legend("topright",
           legend=paste0("4/n = ",round(thr,4),"  |  ",length(inf_idx)," flagged"),
           col=APP_ORANGE,lty=2,lwd=1.6,bty="n",cex=0.74,text.col="#374151")
  })
  
  output$inf_leverage <- renderPlot({
    app_theme()
    m   <- inf_model1(); h <- hatvalues(m)
    p   <- length(coef(m)); n <- length(h); thr <- 2*p/n
    cols <- ifelse(h>thr,APP_ORANGE,APP_GREEN)
    plot(h,type="h",col=cols,lwd=1.2,xlab="Index",ylab="Leverage",
         main="Leverage h\u1d62\u1d62",ylim=c(0,max(h)*1.15))
    add_grid(nx=NA); abline(h=thr,col=APP_RED,lty=2,lwd=1.6)
    hi <- which(h>thr)
    legend("topright",
           legend=paste0("2p/n = ",round(thr,4),"  |  ",length(hi)," high-leverage"),
           col=APP_RED,lty=2,lwd=1.6,bty="n",cex=0.74,text.col="#374151")
  })
  
  output$inf_dffits <- renderPlot({
    app_theme()
    m   <- inf_model1(); d <- dffits(m)
    p   <- length(coef(m)); n <- length(d); thr <- 2*sqrt(p/n)
    cols <- ifelse(abs(d)>thr,APP_RED,APP_PURPLE)
    plot(d,type="h",col=cols,lwd=1.2,xlab="Index",ylab="DFFITS",
         main="DFFITS",ylim=c(min(d)*1.15,max(d)*1.15))
    add_grid(nx=NA)
    abline(h= thr,col=APP_ORANGE,lty=2,lwd=1.6)
    abline(h=-thr,col=APP_ORANGE,lty=2,lwd=1.6)
    flagged <- which(abs(d)>thr)
    legend("topright",
           legend=paste0("\u00b12\u221a(p/n) = \u00b1",round(thr,3),
                         "  |  ",length(flagged)," flagged"),
           col=APP_ORANGE,lty=2,lwd=1.6,bty="n",cex=0.74,text.col="#374151")
  })
  
  output$inf_table <- renderDT({
    im  <- inf_measures()
    im_mat  <- as.data.frame(im$infmat)
    is_inf  <- as.data.frame(im$is.inf)
    flagged_rows <- which(apply(im$is.inf,1,any))
    if (length(flagged_rows)==0)
      return(datatable(data.frame(Note="No observations flagged as influential."),
                       options=list(dom="t"),rownames=FALSE))
    out <- data.frame(
      Obs      = flagged_rows,
      DFFIT    = round(im_mat[flagged_rows,"dffit"],4),
      Cooks_D  = round(im_mat[flagged_rows,"cook.d"],4),
      Leverage = round(im_mat[flagged_rows,"hat"],4),
      COVRATIO = round(im_mat[flagged_rows,"cov.r"],4),
      Flags    = sapply(flagged_rows,function(i)
        paste(names(is_inf)[as.logical(is_inf[i,])],collapse=", ")))
    datatable(out,options=list(scrollX=TRUE,pageLength=15,dom="tip"),
              rownames=FALSE) %>%
      formatStyle("Cooks_D",
                  background=styleColorBar(c(0,max(out$Cooks_D,na.rm=TRUE)),"#fee2e2"),
                  backgroundSize="100% 60%",backgroundRepeat="no-repeat",
                  backgroundPosition="left center")
  })
  
  output$inf_m2_summary_ui <- renderUI({
    m <- inf_model2(); s <- summary(m); n_dropped <- length(inf_flagged_idx())
    HTML(paste0('<div class="result-block">',
                '<div style="font-size:11.5px;color:#9ca3af;margin-bottom:10px;">',
                'Removed ',n_dropped,' influential observation(s) from data.</div>',
                '<div class="rb-section">Coefficients</div>',
                coef_table_html(s,rownames(s$coefficients)),
                '<div class="rb-section">Fit Statistics</div>',
                fit_stats_html(s),'</div>'))
  })
  
  output$inf_m2_resid <- renderPlot({
    app_theme()
    m  <- inf_model2(); fi <- fitted(m); ei <- residuals(m); sg <- sd(ei)
    plot(fi,ei,type="n",xlab="Fitted",ylab="Residual",
         main="Model 2 \u2014 Residuals vs Fitted (after deletion)")
    add_grid(); abline(h=0,col=APP_GRAY,lwd=1.5,lty=2)
    abline(h= 2*sg,col=APP_ORANGE,lwd=1,lty=3)
    abline(h=-2*sg,col=APP_ORANGE,lwd=1,lty=3)
    col_pts <- ifelse(abs(ei)>2*sg,APP_RED,adjustcolor(APP_GREEN,0.55))
    points(fi,ei,pch=21,bg=col_pts,col="white",cex=0.75,lwd=0.3)
  })
  
  output$inf_m3_summary_ui <- renderUI({
    m <- inf_model3(); s <- summary(m)
    HTML(paste0('<div class="result-block">',
                '<div style="font-size:11.5px;color:#9ca3af;margin-bottom:10px;">',
                'rlm( ... , psi = psi.huber )</div>',
                '<div class="rb-section">Coefficients</div>',
                coef_table_html(s,rownames(s$coefficients)),
                '<div class="rb-section">Robust Scale (\u03c3)</div>',
                fit_stats_html(s),
                '<div style="font-size:11.5px;color:#6b7280;margin-top:8px;">',
                'Huber down-weights observations with large residuals using a soft ',
                '\u03c8 function (no zero weights).</div></div>'))
  })
  
  output$inf_huber_weights <- renderPlot({
    app_theme()
    m  <- inf_model3(); rd <- residuals(m); wt <- m$w
    plot(rd,wt,type="n",xlab="Residual",ylab="Huber weight",
         main="Huber: Residuals vs Weights",ylim=c(0,1.05))
    add_grid(); abline(v=0,col=APP_RED,lty=2,lwd=1.4)
    cols <- ifelse(wt<1,APP_ORANGE,adjustcolor(APP_GREEN,0.65))
    points(rd,wt,pch=21,bg=cols,col="white",cex=0.85,lwd=0.4)
    abline(h=1,col=APP_GRAY,lty=3)
    legend("bottomright",
           legend=c("Full weight (=1)","Down-weighted (<1)"),
           col=c(APP_GREEN,APP_ORANGE),pch=21,
           pt.bg=c(adjustcolor(APP_GREEN,0.65),APP_ORANGE),
           bty="n",cex=0.76,text.col="#374151")
  })
  
  output$inf_huber_lowest <- renderDT({
    m  <- inf_model3()
    df <- data.frame(Obs=seq_along(residuals(m)),
                     Residual=round(residuals(m),4),
                     Weight=round(m$w,4))
    df <- df[order(df$Weight),][1:min(10,nrow(df)),]
    datatable(df,options=list(dom="t",pageLength=10),rownames=FALSE) %>%
      formatStyle("Weight",
                  background=styleColorBar(c(0,1),"#fde68a"),
                  backgroundSize="100% 60%",backgroundRepeat="no-repeat",
                  backgroundPosition="left center")
  })
  
  output$inf_m4_summary_ui <- renderUI({
    m <- inf_model4(); s <- summary(m)
    HTML(paste0('<div class="result-block">',
                '<div style="font-size:11.5px;color:#9ca3af;margin-bottom:10px;">',
                'rlm( ... , psi = psi.bisquare )</div>',
                '<div class="rb-section">Coefficients</div>',
                coef_table_html(s,rownames(s$coefficients)),
                '<div class="rb-section">Robust Scale (\u03c3)</div>',
                fit_stats_html(s),
                '<div style="font-size:11.5px;color:#6b7280;margin-top:8px;">',
                'Bisquare (Tukey) is more aggressive: residuals beyond a cutoff ',
                'receive weight = 0 (full rejection).</div></div>'))
  })
  
  output$inf_bisq_weights <- renderPlot({
    app_theme()
    m  <- inf_model4(); rd <- residuals(m); wt <- m$w
    plot(rd,wt,type="n",xlab="Residual",ylab="Bisquare weight",
         main="Bisquare: Residuals vs Weights",ylim=c(0,1.05))
    add_grid()
    abline(v=0,col=APP_RED,lty=2,lwd=1.4)
    abline(h=0,col=APP_RED,lty=3,lwd=1)
    cols <- ifelse(wt<0.001,APP_RED,ifelse(wt<1,APP_ORANGE,adjustcolor(APP_GREEN,0.65)))
    points(rd,wt,pch=21,bg=cols,col="white",cex=0.85,lwd=0.4)
    abline(h=1,col=APP_GRAY,lty=3)
    legend("bottomright",
           legend=c("Full weight (=1)","Down-weighted","Rejected (\u22480)"),
           col=c(APP_GREEN,APP_ORANGE,APP_RED),pch=21,
           pt.bg=c(adjustcolor(APP_GREEN,0.65),APP_ORANGE,APP_RED),
           bty="n",cex=0.76,text.col="#374151")
  })
  
  output$inf_bisq_lowest <- renderDT({
    m  <- inf_model4()
    df <- data.frame(Obs=seq_along(residuals(m)),
                     Residual=round(residuals(m),4),
                     Weight=round(m$w,4))
    df <- df[order(df$Weight),][1:min(10,nrow(df)),]
    datatable(df,options=list(dom="t",pageLength=10),rownames=FALSE) %>%
      formatStyle("Weight",
                  background=styleColorBar(c(0,1),"#fecaca"),
                  backgroundSize="100% 60%",backgroundRepeat="no-repeat",
                  backgroundPosition="left center")
  })
  
  inf_compare_df <- reactive({
    m1 <- inf_model1(); m2 <- inf_model2()
    m3 <- inf_model3(); m4 <- inf_model4()
    coefs <- list(
      `Model 1 (OLS, full)`    = coef(m1),
      `Model 2 (OLS, deleted)` = coef(m2),
      `Model 3 (Huber)`        = coef(m3),
      `Model 4 (Bisquare)`     = coef(m4))
    all_terms <- unique(unlist(lapply(coefs,names)))
    out <- data.frame(Term=all_terms, stringsAsFactors=FALSE)
    for (nm in names(coefs))
      out[[nm]] <- round(coefs[[nm]][all_terms],5)
    sigmas <- c(summary(m1)$sigma,summary(m2)$sigma,
                summary(m3)$sigma,summary(m4)$sigma)
    sigma_row <- data.frame(
      Term=             "Residual Scale (\u03c3)",
      `Model 1 (OLS, full)`    = round(sigmas[1],4),
      `Model 2 (OLS, deleted)` = round(sigmas[2],4),
      `Model 3 (Huber)`        = round(sigmas[3],4),
      `Model 4 (Bisquare)`     = round(sigmas[4],4),
      check.names=FALSE)
    rbind(out,sigma_row)
  })
  
  output$inf_compare <- renderDT({
    datatable(inf_compare_df(),
              options=list(dom="t",pageLength=20,scrollX=TRUE),
              rownames=FALSE)
  })
  
  output$inf_compare_plot <- renderPlot({
    app_theme()
    df  <- inf_compare_df()
    df  <- df[df$Term != "Residual Scale (\u03c3)",,drop=FALSE]
    if (nrow(df)==0) return(NULL)
    par(mar=c(5,4.5,3.2,8))
    mat  <- as.matrix(df[,-1,drop=FALSE])
    rownames(mat) <- df$Term
    cols <- c(APP_BLUE,APP_GREEN,APP_PURPLE,APP_ORANGE)
    bp   <- barplot(t(mat),beside=TRUE,col=cols,border=NA,
                    las=2,cex.names=0.78,
                    main="Coefficient Estimates Across Models",
                    ylab="Estimate")
    add_grid(nx=NA); abline(h=0,col=APP_GRAY,lwd=1)
    legend("topright",inset=c(-0.20,0),legend=colnames(mat),
           fill=cols,border=NA,bty="n",cex=0.74,
           text.col="#374151",xpd=TRUE)
  })
  
  ############################################################
  # ── FEATURE SELECTION ───────────────────────────────────
  ############################################################
  
  fs_data <- reactive({
    req(data(),input$fs_y,input$fs_x)
    validate(need(length(input$fs_x)>=2,"Choose at least two predictors."))
    validate(need(!(input$fs_y %in% input$fs_x),
                  "The response variable cannot also be selected as a predictor."))
    df <- as.data.frame(data()[,c(input$fs_y,input$fs_x),drop=FALSE])
    df <- df[complete.cases(df),,drop=FALSE]
    validate(need(nrow(df)>=length(input$fs_x)+2,
                  "Not enough complete rows for the selected model."))
    df
  })
  
  fs_formula <- reactive({
    req(input$fs_y,input$fs_x)
    reformulate(input$fs_x, response=input$fs_y)
  })
  
  fs_model <- eventReactive(input$run_fs, {
    lm(fs_formula(), data=fs_data())
  })
  
  fs_all <- reactive({
    req(fs_model())
    olsrr::ols_step_all_possible(fs_model())$result
  })
  
  output$fs_full_summary <- renderPrint({ req(fs_model()); summary(fs_model()) })
  output$fs_anova        <- renderPrint({ req(fs_model()); anova(fs_model())   })
  
  output$fs_corrplot <- renderPlot({
    df <- fs_data()
    corrplot.mixed(cor(df),lower="number",upper="circle",
                   outline=TRUE,mar=c(1,1,0,0),
                   tl.cex=0.7,tl.col="black",cl.cex=0.7,
                   cl.ratio=0.2,number.cex=0.8,number.digits=3)
  })
  
  output$fs_vifplot <- renderPlot({
    req(fs_model())
    v <- car::vif(fs_model())
    barplot(v,main="VIF Values",horiz=FALSE,col="steelblue",
            ylim=c(0,max(11,max(v,na.rm=TRUE)+1)))
    abline(h=5,lwd=2,lty=3,col="red")
  })
  
  output$fs_all_reg <- renderDT({
    datatable(fs_all(),options=list(scrollX=TRUE,pageLength=10))
  })
  
  output$fs_best_models <- renderPrint({
    ar <- fs_all()
    cat("Best by minimizing MSE:           ",ar$predictors[which.min(ar$msep)],"\n")
    cat("Best by maximizing Adj. R\u00b2:        ",ar$predictors[which.max(ar$adjr)],"\n")
    cat("Best by maximizing Pred. R\u00b2:       ",ar$predictors[which.max(ar$predrsq)],"\n")
    ar$cpp <- abs(ar$cp-(ar$n+1))
    cat("Best by Cp closest to p:           ",ar$predictors[which.min(ar$cpp)],"\n")
    cat("Best by minimizing AIC:            ",ar$predictors[which.min(ar$aic)],"\n")
    cat("Best by minimizing BIC:            ",ar$predictors[which.min(ar$sbc)],"\n")
  })
  
  output$fs_cp_plot <- renderPlot({
    ar <- fs_all()
    plot(x=ar$n+1,y=ar$cp,
         ylim=c(0,max(20,max(ar$cp,na.rm=TRUE))),
         ylab="Mallow's Cp",xlab="p = k + 1",
         pch=19,col="steelblue")
    abline(a=0,b=1,col="red",lwd=2)
  })
  
  output$fs_forward  <- renderPrint({ req(fs_model()); olsrr::ols_step_forward_p(fs_model(),  penter=0.1,  details=TRUE) })
  output$fs_backward <- renderPrint({ req(fs_model()); olsrr::ols_step_backward_p(fs_model(), prem=0.15,   details=TRUE) })
  output$fs_stepwise <- renderPrint({ req(fs_model()); olsrr::ols_step_both_p(fs_model(),     pent=0.1,prem=0.15,details=TRUE) })
  
  output$fs_final_model <- renderPrint({
    req(data()); df <- as.data.frame(data())
    if (all(c("y","x1","x2","x4") %in% names(df))) {
      final.model <- lm(y~x1+x2+x4,data=df)
      print(summary(final.model)); cat("\nVIF:\n"); print(car::vif(final.model))
    } else {
      cat("Final model y ~ x1 + x2 + x4 requires columns named y, x1, x2, and x4.\n")
      cat("Use the best model results above if your data has different column names.\n")
    }
  })
  
  ############################################################
  # ── MULTICOLLINEARITY ───────────────────────────────────
  ############################################################
  
  mc_df <- reactive({
    req(data(),input$mc_y,input$mc_x1,input$mc_x2,input$mc_x3)
    validate(need(length(unique(c(input$mc_y,input$mc_x1,input$mc_x2,input$mc_x3)))==4,
                  "Y, X\u2081, X\u2082, X\u2083 must all be different columns."))
    df <- data()[,c(input$mc_y,input$mc_x1,input$mc_x2,input$mc_x3)]
    names(df) <- c("y","T","H","C")
    df <- df[complete.cases(df),]
    validate(need(nrow(df)>=10,
                  "Need at least 10 complete observations for a 9-term quadratic surface."))
    df
  })
  
  mc_model_raw  <- reactive({
    df <- mc_df()
    lm(y~T+H+C+I(T^2)+I(H^2)+I(C^2)+I(T*H)+I(T*C)+I(H*C),data=df)
  })
  
  mc_df_cent <- reactive({
    df <- mc_df(); n <- nrow(df)
    scale_ul <- function(v) (v-mean(v))/sqrt((n-1)*var(v))
    data.frame(y=df$y,x1=scale_ul(df$T),x2=scale_ul(df$H),x3=scale_ul(df$C))
  })
  
  mc_model_cent <- reactive({
    dfc <- mc_df_cent()
    lm(y~x1+x2+x3+I(x1^2)+I(x2^2)+I(x3^2)+I(x1*x2)+I(x1*x3)+I(x2*x3),data=dfc)
  })
  
  mc_X <- reactive({
    df <- mc_df()
    model.matrix(~T+H+C+I(T^2)+I(H^2)+I(C^2)+I(T*H)+I(T*C)+I(H*C),data=df)[,-1]
  })
  
  mc_cv_ridge <- reactive({
    set.seed(input$mc_seed)
    cv.glmnet(mc_X(),mc_df()$y,alpha=0,standardize=TRUE,nfolds=input$mc_folds,
              lambda=exp(seq(log(0.001),log(10),length.out=100)))
  })
  
  mc_cv_lasso <- reactive({
    set.seed(input$mc_seed)
    cv.glmnet(mc_X(),mc_df()$y,alpha=1,standardize=TRUE,nfolds=input$mc_folds,
              lambda=exp(seq(log(0.0001),log(5),length.out=100)))
  })
  
  output$mc_raw_summary_ui  <- renderUI({
    m <- mc_model_raw();  s <- summary(m)
    HTML(paste0('<div class="result-block"><div class="rb-section">Coefficients</div>',
                coef_table_html(s,rownames(s$coefficients)),
                '<div class="rb-section">Fit Statistics</div>',fit_stats_html(s),'</div>'))
  })
  output$mc_cent_summary_ui <- renderUI({
    m <- mc_model_cent(); s <- summary(m)
    HTML(paste0('<div class="result-block"><div class="rb-section">Coefficients</div>',
                coef_table_html(s,rownames(s$coefficients)),
                '<div class="rb-section">Fit Statistics</div>',fit_stats_html(s),'</div>'))
  })
  
  vif_html_block <- function(vif_vals) {
    max_vif <- max(vif_vals); rows <- ""
    for (nm in names(vif_vals)) {
      v   <- vif_vals[[nm]]
      pct <- min(round(v/max(11,max_vif*1.1)*100),100)
      cls <- if(v<5)"vif-ok" else if(v<10)"vif-warn" else "vif-high"
      rows <- paste0(rows,
                     '<div class="rb-vif-row"><span class="rb-vif-name">',nm,'</span>',
                     '<div class="rb-vif-bar-wrap"><div class="rb-vif-bar ',cls,
                     '" style="width:',pct,'%;"></div></div>',
                     '<span class="rb-vif-val">',fmt_num(v,2),'</span></div>')
    }
    note <- if(max_vif<5)
      '<div style="font-size:11px;color:#6b7280;margin-top:8px;">All VIF &lt; 5 \u2014 no multicollinearity concern</div>'
    else if(max_vif<10)
      '<div style="font-size:11px;color:#92400e;margin-top:8px;">VIF 5\u201310 \u2014 moderate multicollinearity</div>'
    else
      '<div style="font-size:11px;color:#b91c1c;margin-top:8px;">VIF &gt; 10 \u2014 severe multicollinearity</div>'
    paste0('<div class="result-block">',rows,note,'</div>')
  }
  
  output$mc_vif_raw_ui  <- renderUI({ HTML(vif_html_block(car::vif(mc_model_raw())))  })
  output$mc_vif_cent_ui <- renderUI({ HTML(vif_html_block(car::vif(mc_model_cent()))) })
  
  vif_barplot_mc <- function(vifs,title) {
    app_theme(); par(mar=c(7.5,3.8,3.2,1.4))
    cols <- ifelse(vifs<5,APP_GREEN,ifelse(vifs<10,"#f59e0b",APP_RED))
    ymax <- max(c(vifs,11))*1.05
    bp   <- barplot(vifs,col=cols,border=NA,las=2,cex.names=0.72,
                    ylim=c(0,ymax),main=title,ylab="VIF")
    add_grid(nx=NA)
    abline(h=5, col=APP_RED,    lwd=1.6,lty=3)
    abline(h=10,col=APP_ORANGE, lwd=1.4,lty=3)
    text(bp,vifs+ymax*0.02,labels=formatC(vifs,format="f",digits=1),
         cex=0.65,col="#374151",font=2)
  }
  output$mc_vif_raw_plot  <- renderPlot({ vif_barplot_mc(car::vif(mc_model_raw()),  "VIFs \u2014 Raw Quadratic Surface")       })
  output$mc_vif_cent_plot <- renderPlot({ vif_barplot_mc(car::vif(mc_model_cent()), "VIFs \u2014 Centered & Unit-Length Scaled") })
  
  cv_themed_plot <- function(cvfit,ttl,accent) {
    app_theme()
    lam_log <- log(cvfit$lambda); cvm <- cvfit$cvm
    cvup <- cvfit$cvup; cvlo <- cvfit$cvlo
    plot(lam_log,cvm,type="n",xlab=expression(log(lambda)),
         ylab="Mean CV MSE",main=ttl,ylim=range(c(cvlo,cvup)))
    add_grid()
    polygon(c(lam_log,rev(lam_log)),c(cvlo,rev(cvup)),
            col=adjustcolor(accent,0.12),border=NA)
    lines(lam_log,cvm,col=accent,lwd=2.4)
    points(lam_log,cvm,pch=21,bg=adjustcolor(accent,0.55),
           col=accent,cex=0.55,lwd=0.4)
    abline(v=log(cvfit$lambda.min),col=APP_ORANGE,lty=2,lwd=1.6)
    abline(v=log(cvfit$lambda.1se),col=APP_GRAY,  lty=3,lwd=1.4)
    legend("topleft",
           legend=c(paste0("\u03bb.min = ",formatC(cvfit$lambda.min,format="g",digits=3)),
                    paste0("\u03bb.1se = ",formatC(cvfit$lambda.1se,format="g",digits=3))),
           col=c(APP_ORANGE,APP_GRAY),lty=c(2,3),lwd=c(1.6,1.4),
           bty="n",cex=0.76,text.col="#374151")
    mtext(paste0("Min CV-MSE = ",formatC(min(cvm),format="f",digits=4)),
          side=3,line=0.1,cex=0.68,col="#9ca3af")
  }
  output$mc_ridge_cv_plot <- renderPlot({ cv_themed_plot(mc_cv_ridge(),"Ridge \u2014 10-fold CV",APP_PURPLE) })
  output$mc_lasso_cv_plot <- renderPlot({ cv_themed_plot(mc_cv_lasso(),"LASSO \u2014 10-fold CV",APP_GREEN)  })
  
  glmnet_coef_table <- function(cvfit,label_color) {
    coefs <- as.matrix(coef(cvfit,s="lambda.min"))
    nms <- rownames(coefs); est <- coefs[,1]; rows <- ""
    for (i in seq_along(est)) {
      zero_flag <- if(abs(est[i])<1e-10)
        '<span class="sig-badge sig-ns">shrunk to 0</span>'
      else sprintf('<span class="sig-badge sig-3star" style="background:%s20;color:%s;">active</span>',
                   label_color,label_color)
      rows <- paste0(rows,sprintf('<tr><td>%s</td><td>%s</td><td>%s</td></tr>',
                                  nms[i],fmt_num(est[i],5),zero_flag))
    }
    paste0('<table class="rb-coef-table">',
           '<thead><tr><th>Term</th><th>Estimate</th><th>Status</th></tr></thead>',
           '<tbody>',rows,'</tbody></table>')
  }
  
  output$mc_ridge_coef_ui <- renderUI({
    cv <- mc_cv_ridge()
    HTML(paste0('<div class="result-block">',
                '<div class="rb-row"><span class="rb-key">Best \u03bb (min CV-MSE)</span>',
                '<span class="rb-val">',fmt_num(cv$lambda.min,5),'</span></div>',
                '<div class="rb-row"><span class="rb-key">Min CV-MSE</span>',
                '<span class="rb-val">',fmt_num(min(cv$cvm),4),'</span></div>',
                '<div class="rb-section">Coefficients @ \u03bb.min</div>',
                glmnet_coef_table(cv,"#7c3aed"),'</div>'))
  })
  
  output$mc_lasso_coef_ui <- renderUI({
    cv    <- mc_cv_lasso()
    coefs <- as.matrix(coef(cv,s="lambda.min"))
    n_zero <- sum(abs(coefs[-1,1])<1e-10)
    n_act  <- nrow(coefs)-1-n_zero
    HTML(paste0('<div class="result-block">',
                '<div class="rb-row"><span class="rb-key">Best \u03bb (min CV-MSE)</span>',
                '<span class="rb-val">',fmt_num(cv$lambda.min,5),'</span></div>',
                '<div class="rb-row"><span class="rb-key">Min CV-MSE</span>',
                '<span class="rb-val">',fmt_num(min(cv$cvm),4),'</span></div>',
                '<div class="rb-row"><span class="rb-key">Active / Shrunk to 0</span>',
                '<span class="rb-val">',n_act,' / ',n_zero,'</span></div>',
                '<div class="rb-section">Coefficients @ \u03bb.min</div>',
                glmnet_coef_table(cv,APP_GREEN),'</div>'))
  })
  
  output$mc_predictions_ui <- renderUI({
    req(input$mc_new_x1,input$mc_new_x2,input$mc_new_x3)
    pv <- function(s){v<-suppressWarnings(as.numeric(trimws(unlist(strsplit(s,",")))));v[!is.na(v)]}
    a  <- pv(input$mc_new_x1); b <- pv(input$mc_new_x2); c0 <- pv(input$mc_new_x3)
    validate(need(length(a)>0 && length(a)==length(b) && length(a)==length(c0),
                  "Enter equal-length comma-separated values for X\u2081, X\u2082, X\u2083."))
    nd    <- data.frame(T=a,H=b,C=c0)
    new_X <- model.matrix(~T+H+C+I(T^2)+I(H^2)+I(C^2)+I(T*H)+I(T*C)+I(H*C),data=nd)[,-1]
    pr_r  <- as.numeric(predict(mc_cv_ridge(),newx=new_X,s="lambda.min"))
    pr_l  <- as.numeric(predict(mc_cv_lasso(),newx=new_X,s="lambda.min"))
    rows  <- ""
    for (i in seq_len(nrow(nd)))
      rows <- paste0(rows,sprintf('<tr><td>(%s, %s, %s)</td><td>%s</td><td>%s</td></tr>',
                                  nd$T[i],nd$H[i],nd$C[i],fmt_num(pr_r[i],4),fmt_num(pr_l[i],4)))
    HTML(paste0('<div class="result-block">',
                '<table class="rb-pred-table"><thead><tr>',
                '<th>(X\u2081, X\u2082, X\u2083)</th><th>Ridge \u0177</th><th>LASSO \u0177</th>',
                '</tr></thead><tbody>',rows,'</tbody></table></div>'))
  })
  
  mc_cv_rmses <- reactive({
    set.seed(input$mc_seed)
    df  <- mc_df(); dfc <- mc_df_cent()
    rmse_raw  <- sqrt(boot::cv.glm(df,  glm(formula(mc_model_raw()),  data=df),  K=input$mc_folds)$delta[1])
    rmse_cent <- sqrt(boot::cv.glm(dfc, glm(formula(mc_model_cent()), data=dfc), K=input$mc_folds)$delta[1])
    rmse_r    <- sqrt(min(mc_cv_ridge()$cvm))
    rmse_l    <- sqrt(min(mc_cv_lasso()$cvm))
    c(`OLS (raw)`=rmse_raw,`OLS (centered)`=rmse_cent,Ridge=rmse_r,LASSO=rmse_l)
  })
  
  output$mc_compare_plot <- renderPlot({
    rm <- mc_cv_rmses(); app_theme(); par(mar=c(5,4,3.2,1.4))
    cols <- c(APP_BLUE,"#60a5fa",APP_PURPLE,APP_GREEN)
    bp   <- barplot(rm,col=cols,border=NA,
                    ylab=paste0(input$mc_folds,"-fold CV RMSE"),
                    main="Model Comparison \u2014 CV-RMSE (lower is better)",
                    ylim=c(0,max(rm)*1.18),las=1,cex.names=0.85)
    add_grid(nx=NA)
    text(bp,rm+max(rm)*0.025,labels=formatC(rm,format="f",digits=4),
         cex=0.78,col="#0f1f12",font=2)
    best <- which.min(rm)
    text(bp[best],rm[best]/2,labels="\u2605 best",cex=0.85,col="white",font=2)
  })
  
  output$mc_compare_ui <- renderUI({
    rm <- mc_cv_rmses(); best <- names(rm)[which.min(rm)]; rows <- ""
    for (i in seq_along(rm)) {
      flag <- if(names(rm)[i]==best)
        '<span class="sig-badge sig-3star">\u2605 best</span>'
      else '<span class="sig-badge sig-ns">\u2014</span>'
      rows <- paste0(rows,sprintf('<tr><td>%s</td><td>%s</td><td>%s</td></tr>',
                                  names(rm)[i],fmt_num(rm[[i]],5),flag))
    }
    HTML(paste0('<div class="result-block">',
                '<table class="rb-coef-table"><thead><tr>',
                '<th>Model</th><th>CV-RMSE</th><th>Verdict</th></tr></thead>',
                '<tbody>',rows,'</tbody></table>',
                '<div class="rb-decision rb-decision-sig" style="margin-top:14px;">',
                '<span class="rb-decision-icon">\u2713</span> Lowest CV-RMSE: <b>&nbsp;',best,'</b></div>',
                '<div style="font-size:11px;color:#6b7280;margin-top:10px;line-height:1.55;">',
                'Note: <code>set.seed(',input$mc_seed,')</code> applied before each CV.</div></div>'))
  })
  
  ############################################################
  # ── TOPIC 7 — POLYNOMIAL & SPLINES ─────────────────────
  ############################################################
  
  # ── Hardwood reactive data ──
  hw_df <- reactive({
    req(data(), input$hw_x, input$hw_y)
    validate(need(input$hw_x != input$hw_y, "X and Y must differ."))
    df <- data()[, c(input$hw_x, input$hw_y)]
    names(df) <- c("x","y")
    df[complete.cases(df),]
  })
  
  hw_xc <- reactive({
    hw_df()$x - mean(hw_df()$x)
  })
  
  hw_m1 <- reactive({ df <- hw_df();                          lm(y ~ x,                    data=df) })
  hw_m2 <- reactive({ df <- hw_df();                          lm(y ~ x + I(x^2),           data=df) })
  hw_m3 <- reactive({ df <- hw_df(); df$xc <- hw_xc();        lm(y ~ xc + I(xc^2),         data=df) })
  hw_m4 <- reactive({ df <- hw_df();                          lm(y ~ x + I(x^2) + I(x^3),  data=df) })
  hw_m5 <- reactive({ df <- hw_df(); df$xc <- hw_xc();        lm(y ~ xc + I(xc^2) + I(xc^3), data=df) })
  
  # Scatter plots — Hardwood
  output$hw_scatter <- renderPlot({
    df <- hw_df(); app_theme()
    plot(df$x, df$y, pch=21, bg=adjustcolor(APP_BLUE,0.55), col=APP_BLUE,
         cex=0.9, lwd=0.4, xlab=input$hw_x, ylab=input$hw_y,
         main=paste("Scatter:",input$hw_y,"vs",input$hw_x))
    add_grid()
    mtext(paste0("n = ",nrow(df)), side=3, line=0.1, cex=0.68, col="#9ca3af")
  })
  
  output$hw_m1_scatter <- renderPlot({
    df <- hw_df(); m <- hw_m1(); app_theme()
    plot(df$x, df$y, pch=21, bg=adjustcolor(APP_BLUE,0.55), col=APP_BLUE,
         cex=0.9, lwd=0.4, xlab=input$hw_x, ylab=input$hw_y,
         main="Model 1 \u2014 Linear Fit")
    add_grid(); abline(m, col=APP_ORANGE, lwd=2.2)
    mtext(paste0("R\u00b2 = ",round(summary(m)$r.squared,4)),
          side=3, line=0.1, cex=0.68, col="#9ca3af")
  })
  
  output$hw_m2_scatter <- renderPlot({
    df <- hw_df(); m <- hw_m2(); app_theme()
    xseq <- seq(min(df$x), max(df$x), length=200)
    plot(df$x, df$y, pch=21, bg=adjustcolor(APP_BLUE,0.55), col=APP_BLUE,
         cex=0.9, lwd=0.4, xlab=input$hw_x, ylab=input$hw_y,
         main="Model 2 \u2014 Quadratic Fit")
    add_grid()
    lines(xseq, predict(m, data.frame(x=xseq)), col=APP_ORANGE, lwd=2.2)
    mtext(paste0("R\u00b2 = ",round(summary(m)$r.squared,4)),
          side=3, line=0.1, cex=0.68, col="#9ca3af")
  })
  
  output$hw_m3_scatter <- renderPlot({
    df <- hw_df(); df$xc <- hw_xc(); m <- hw_m3(); app_theme()
    xseq <- seq(min(df$xc), max(df$xc), length=200)
    plot(df$xc, df$y, pch=21, bg=adjustcolor(APP_BLUE,0.55), col=APP_BLUE,
         cex=0.9, lwd=0.4, xlab=paste0(input$hw_x," (centered)"), ylab=input$hw_y,
         main="Model 3 \u2014 Quadratic Fit (Centered)")
    add_grid()
    lines(xseq, predict(m, data.frame(xc=xseq)), col=APP_GREEN, lwd=2.2)
    mtext(paste0("R\u00b2 = ",round(summary(m)$r.squared,4)),
          side=3, line=0.1, cex=0.68, col="#9ca3af")
  })
  
  output$hw_m4_scatter <- renderPlot({
    df <- hw_df(); m <- hw_m4(); app_theme()
    xseq <- seq(min(df$x), max(df$x), length=200)
    plot(df$x, df$y, pch=21, bg=adjustcolor(APP_BLUE,0.55), col=APP_BLUE,
         cex=0.9, lwd=0.4, xlab=input$hw_x, ylab=input$hw_y,
         main="Model 4 \u2014 Cubic Fit")
    add_grid()
    lines(xseq, predict(m, data.frame(x=xseq)), col=APP_ORANGE, lwd=2.2)
    mtext(paste0("R\u00b2 = ",round(summary(m)$r.squared,4)),
          side=3, line=0.1, cex=0.68, col="#9ca3af")
  })
  
  output$hw_m5_scatter <- renderPlot({
    df <- hw_df(); df$xc <- hw_xc(); m <- hw_m5(); app_theme()
    xseq <- seq(min(df$xc), max(df$xc), length=200)
    plot(df$xc, df$y, pch=21, bg=adjustcolor(APP_BLUE,0.55), col=APP_BLUE,
         cex=0.9, lwd=0.4, xlab=paste0(input$hw_x," (centered)"), ylab=input$hw_y,
         main="Model 5 \u2014 Cubic Fit (Centered)")
    add_grid()
    lines(xseq, predict(m, data.frame(xc=xseq)), col=APP_GREEN, lwd=2.2)
    mtext(paste0("R\u00b2 = ",round(summary(m)$r.squared,4)),
          side=3, line=0.1, cex=0.68, col="#9ca3af")
  })
  
  # Residual plots — Hardwood
  output$hw_m1_resid <- renderPlot({ poly_resid_plot(hw_m1(), "Model 1 \u2014 Linear Residuals")          })
  output$hw_m2_resid <- renderPlot({ poly_resid_plot(hw_m2(), "Model 2 \u2014 Quadratic Residuals")       })
  output$hw_m3_resid <- renderPlot({ poly_resid_plot(hw_m3(), "Model 3 \u2014 Quadratic Centered Residuals") })
  output$hw_m4_resid <- renderPlot({ poly_resid_plot(hw_m4(), "Model 4 \u2014 Cubic Residuals")           })
  output$hw_m5_resid <- renderPlot({ poly_resid_plot(hw_m5(), "Model 5 \u2014 Cubic Centered Residuals")  })
  
  # VIF plots — Hardwood
  output$hw_m2_vif <- renderPlot({ vif_barplot_t7(car::vif(hw_m2()), "VIF \u2014 Quadratic (x, x\u00b2)") })
  output$hw_m3_vif <- renderPlot({ vif_barplot_t7(car::vif(hw_m3()), "VIF \u2014 Quadratic Centered (xc, xc\u00b2)") })
  output$hw_m4_vif <- renderPlot({ vif_barplot_t7(car::vif(hw_m4()), "VIF \u2014 Cubic (x, x\u00b2, x\u00b3)") })
  output$hw_m5_vif <- renderPlot({ vif_barplot_t7(car::vif(hw_m5()), "VIF \u2014 Cubic Centered (xc, xc\u00b2, xc\u00b3)") })
  
  # Summary UIs — Hardwood
  output$hw_m1_ui <- renderUI({ poly_model_summary_html(hw_m1(), paste0("lm(y ~ x)")) })
  output$hw_m2_ui <- renderUI({ poly_model_summary_html(hw_m2(), paste0("lm(y ~ x + I(x^2))")) })
  output$hw_m3_ui <- renderUI({ poly_model_summary_html(hw_m3(), paste0("lm(y ~ xc + I(xc^2))  [xc = x - mean(x)]")) })
  output$hw_m4_ui <- renderUI({ poly_model_summary_html(hw_m4(), paste0("lm(y ~ x + I(x^2) + I(x^3))")) })
  output$hw_m5_ui <- renderUI({ poly_model_summary_html(hw_m5(), paste0("lm(y ~ xc + I(xc^2) + I(xc^3))  [xc = x - mean(x)]")) })
  
  # Comparison table — Hardwood
  output$hw_compare_ui <- renderUI({
    models <- list(
      "Model 1 \u2014 Linear"           = hw_m1(),
      "Model 2 \u2014 Quadratic"        = hw_m2(),
      "Model 3 \u2014 Quadratic Centered"= hw_m3(),
      "Model 4 \u2014 Cubic"            = hw_m4(),
      "Model 5 \u2014 Cubic Centered"   = hw_m5())
    rows <- ""
    for (nm in names(models)) {
      s   <- summary(models[[nm]])
      r2  <- fmt_num(s$r.squared,4)
      ar2 <- fmt_num(s$adj.r.squared,4)
      rse <- fmt_num(s$sigma,4)
      rows <- paste0(rows,sprintf('<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>',
                                  nm,r2,ar2,rse))
    }
    HTML(paste0('<div class="result-block">',
                '<table class="rb-coef-table"><thead><tr>',
                '<th>Model</th><th>R\u00b2</th><th>Adj. R\u00b2</th><th>RSE</th>',
                '</tr></thead><tbody>',rows,'</tbody></table></div>'))
  })
  
  # ── Voltage reactive data ──
  vt_df <- reactive({
    req(data(), input$vt_x, input$vt_y)
    validate(need(input$vt_x != input$vt_y, "X and Y must differ."))
    df <- data()[, c(input$vt_x, input$vt_y)]
    names(df) <- c("x","y")
    df <- df[complete.cases(df),]
    df <- df[order(df$x),]
    df
  })
  
  vt_m1 <- reactive({ lm(y ~ x,                            data=vt_df()) })
  vt_m2 <- reactive({ lm(y ~ x + I(x^2),                   data=vt_df()) })
  vt_m3 <- reactive({ lm(y ~ x + I(x^2) + I(x^3),          data=vt_df()) })
  vt_m4 <- reactive({ lm(y ~ x + I(x^2) + I(x^3) + I(x^4), data=vt_df()) })
  vt_m5 <- reactive({ lm(y ~ bs(x, knots=10, degree=1, intercept=TRUE), data=vt_df()) })
  vt_m6 <- reactive({ lm(y ~ bs(x, knots=c(input$vt_k2, input$vt_k3), degree=1), data=vt_df()) })
  vt_m7 <- reactive({ lm(y ~ bs(x, knots=c(input$vt_k2, input$vt_k3), degree=2), data=vt_df()) })
  vt_m8 <- reactive({ lm(y ~ bs(x, knots=c(input$vt_k2, input$vt_k3), degree=3), data=vt_df()) })
  
  # Scatter plot helper for voltage
  vt_scatter_fn <- function(df, m, main_str, col=APP_BLUE) {
    app_theme()
    xseq <- seq(min(df$x), max(df$x), length=300)
    pred <- tryCatch(predict(m, data.frame(x=xseq)), error=function(e) rep(NA,300))
    plot(df$x, df$y, pch=21, bg=adjustcolor(col,0.55), col=col,
         cex=0.85, lwd=0.4, xlab=input$vt_x, ylab=input$vt_y, main=main_str)
    add_grid()
    if (!any(is.na(pred))) lines(xseq, pred, col=APP_ORANGE, lwd=2.3)
    s <- summary(m)
    mtext(paste0("R\u00b2 = ",round(s$r.squared,4),
                 "  |  Adj.R\u00b2 = ",round(s$adj.r.squared,4)),
          side=3, line=0.1, cex=0.68, col="#9ca3af")
  }
  
  output$vt_scatter   <- renderPlot({
    df <- vt_df(); app_theme()
    plot(df$x, df$y, pch=21, bg=adjustcolor(APP_BLUE,0.55), col=APP_BLUE,
         cex=0.85, lwd=0.4, xlab=input$vt_x, ylab=input$vt_y,
         main="Voltage Drop vs Time \u2014 Original Data")
    add_grid()
    abline(v=c(input$vt_k2, input$vt_k3),
           col=adjustcolor(APP_RED,0.4), lty=2, lwd=1.4)
    legend("topright",
           legend=paste0("Knots at x = ",input$vt_k2," and x = ",input$vt_k3),
           col=adjustcolor(APP_RED,0.4), lty=2, lwd=1.4,
           bty="n", cex=0.76, text.col="#374151")
  })
  
  output$vt_m1_scatter <- renderPlot({ vt_scatter_fn(vt_df(), vt_m1(), "Model 1 \u2014 Linear")   })
  output$vt_m2_scatter <- renderPlot({ vt_scatter_fn(vt_df(), vt_m2(), "Model 2 \u2014 Quadratic") })
  output$vt_m3_scatter <- renderPlot({ vt_scatter_fn(vt_df(), vt_m3(), "Model 3 \u2014 Cubic")     })
  output$vt_m4_scatter <- renderPlot({ vt_scatter_fn(vt_df(), vt_m4(), "Model 4 \u2014 Quartic")   })
  output$vt_m5_scatter <- renderPlot({ vt_scatter_fn(vt_df(), vt_m5(), "Model 5 \u2014 Linear Spline (1 knot at x=10)") })
  output$vt_m6_scatter <- renderPlot({ vt_scatter_fn(vt_df(), vt_m6(), paste0("Model 6 \u2014 Linear Spline (k2=",input$vt_k2,", k3=",input$vt_k3,")")) })
  output$vt_m7_scatter <- renderPlot({ vt_scatter_fn(vt_df(), vt_m7(), paste0("Model 7 \u2014 Quadratic Spline (k2=",input$vt_k2,", k3=",input$vt_k3,")")) })
  output$vt_m8_scatter <- renderPlot({ vt_scatter_fn(vt_df(), vt_m8(), paste0("Model 8 \u2014 Cubic Spline (k2=",input$vt_k2,", k3=",input$vt_k3,")")) })
  
  output$vt_m1_resid <- renderPlot({ poly_resid_plot(vt_m1(), "Model 1 \u2014 Linear Residuals")   })
  output$vt_m2_resid <- renderPlot({ poly_resid_plot(vt_m2(), "Model 2 \u2014 Quadratic Residuals") })
  output$vt_m3_resid <- renderPlot({ poly_resid_plot(vt_m3(), "Model 3 \u2014 Cubic Residuals")     })
  output$vt_m4_resid <- renderPlot({ poly_resid_plot(vt_m4(), "Model 4 \u2014 Quartic Residuals")   })
  output$vt_m5_resid <- renderPlot({ poly_resid_plot(vt_m5(), "Model 5 \u2014 Linear Spline Residuals")    })
  output$vt_m6_resid <- renderPlot({ poly_resid_plot(vt_m6(), "Model 6 \u2014 Linear Spline 2-knot Residuals")    })
  output$vt_m7_resid <- renderPlot({ poly_resid_plot(vt_m7(), "Model 7 \u2014 Quadratic Spline Residuals") })
  output$vt_m8_resid <- renderPlot({ poly_resid_plot(vt_m8(), "Model 8 \u2014 Cubic Spline Residuals")     })
  
  output$vt_m1_ui <- renderUI({ poly_model_summary_html(vt_m1(), "lm(y ~ x)") })
  output$vt_m2_ui <- renderUI({ poly_model_summary_html(vt_m2(), "lm(y ~ x + I(x^2))") })
  output$vt_m3_ui <- renderUI({ poly_model_summary_html(vt_m3(), "lm(y ~ x + I(x^2) + I(x^3))") })
  output$vt_m4_ui <- renderUI({ poly_model_summary_html(vt_m4(), "lm(y ~ x + I(x^2) + I(x^3) + I(x^4))") })
  output$vt_m5_ui <- renderUI({ poly_model_summary_html(vt_m5(), "lm(y ~ bs(x, knots=10, degree=1))") })
  output$vt_m6_ui <- renderUI({ poly_model_summary_html(vt_m6(), paste0("lm(y ~ bs(x, knots=c(",input$vt_k2,",",input$vt_k3,"), degree=1))")) })
  output$vt_m7_ui <- renderUI({ poly_model_summary_html(vt_m7(), paste0("lm(y ~ bs(x, knots=c(",input$vt_k2,",",input$vt_k3,"), degree=2))")) })
  output$vt_m8_ui <- renderUI({ poly_model_summary_html(vt_m8(), paste0("lm(y ~ bs(x, knots=c(",input$vt_k2,",",input$vt_k3,"), degree=3))")) })
  
  output$vt_compare_plot <- renderPlot({
    app_theme()
    mods <- list(vt_m1(),vt_m2(),vt_m3(),vt_m4(),vt_m5(),vt_m6(),vt_m7(),vt_m8())
    r2   <- sapply(mods, function(m) summary(m)$r.squared)
    nms  <- c("M1\nLinear","M2\nQuadratic","M3\nCubic","M4\nQuartic",
              "M5\nLin.Spl\n1knot","M6\nLin.Spl\n2knot",
              "M7\nQuad.Spl","M8\nCubic.Spl")
    cols <- c(rep(APP_BLUE,4), rep(APP_GREEN,4))
    par(mar=c(6,4,3.2,1.4))
    bp <- barplot(r2, names.arg=nms, col=cols, border=NA,
                  ylim=c(0,1.1), ylab="R\u00b2",
                  main="R\u00b2 Comparison \u2014 All 8 Models",
                  las=2, cex.names=0.72)
    add_grid(nx=NA)
    abline(h=0.95, col=APP_ORANGE, lty=2, lwd=1.4)
    text(bp, r2+0.02, labels=round(r2,3), cex=0.70, col="#0f1f12", font=2)
    best <- which.max(r2)
    text(bp[best], r2[best]/2, labels="\u2605", cex=1.4, col="white", font=2)
    legend("bottomright",
           legend=c("Polynomial","Spline","R\u00b2 = 0.95 ref."),
           fill=c(APP_BLUE,APP_GREEN,NA), border=NA,
           lty=c(NA,NA,2), col=c(NA,NA,APP_ORANGE),
           bty="n", cex=0.76, text.col="#374151")
  })
  
  output$vt_compare_ui <- renderUI({
    mods <- list(vt_m1(),vt_m2(),vt_m3(),vt_m4(),vt_m5(),vt_m6(),vt_m7(),vt_m8())
    nms  <- c("Model 1 \u2014 Linear","Model 2 \u2014 Quadratic",
              "Model 3 \u2014 Cubic","Model 4 \u2014 Quartic",
              "Model 5 \u2014 Linear Spline (1 knot)",
              "Model 6 \u2014 Linear Spline (2 knots)",
              "Model 7 \u2014 Quadratic Spline","Model 8 \u2014 Cubic Spline")
    rows <- ""
    best_r2 <- which.max(sapply(mods, function(m) summary(m)$r.squared))
    for (i in seq_along(mods)) {
      s   <- summary(mods[[i]])
      r2  <- fmt_num(s$r.squared,4)
      ar2 <- fmt_num(s$adj.r.squared,4)
      rse <- fmt_num(s$sigma,4)
      flag <- if(i==best_r2)
        '<span class="sig-badge sig-3star">\u2605 best R\u00b2</span>' else ""
      rows <- paste0(rows,sprintf(
        '<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>',
        nms[i],r2,ar2,rse,flag))
    }
    HTML(paste0('<div class="result-block">',
                '<table class="rb-coef-table"><thead><tr>',
                '<th>Model</th><th>R\u00b2</th><th>Adj. R\u00b2</th><th>RSE</th><th></th>',
                '</tr></thead><tbody>',rows,'</tbody></table></div>'))
  })
  
  ############################################################
  # ── GLM ─────────────────────────────────────────────────
  ############################################################
  
  bayes_df <- reactive({
    req(data(), input$bayes_x, input$bayes_y)
    validate(need(input$bayes_x != input$bayes_y, "X and Y must differ."))
    df <- data()[, c(input$bayes_x, input$bayes_y), drop=FALSE]
    names(df) <- c("x","y")
    df <- df[complete.cases(df),]
    df$x <- as.numeric(df$x)
    df$y <- as.numeric(df$y)
    validate(need(nrow(df) >= 5, "Need at least 5 complete observations."))
    df
  })
  
  bayes_models <- eventReactive(input$run_bayes, {
    df <- bayes_df()
    set.seed(input$bayes_seed)
    ols <- lm(y ~ x, data=df)
    draws <- NULL
    method <- "Conjugate Bayesian linear regression"
    stan_summary <- NULL
    stan_interval <- NULL
    
    if (requireNamespace("rstanarm", quietly=TRUE)) {
      stan_fit <- tryCatch(
        rstanarm::stan_glm(y ~ x, data=df, family=gaussian(),
                           chains=4, iter=input$bayes_iter,
                           seed=input$bayes_seed, refresh=0),
        error=function(e) NULL
      )
      if (!is.null(stan_fit)) {
        method <- "rstanarm::stan_glm"
        mat <- as.matrix(stan_fit)
        draws <- data.frame(intercept=mat[, "(Intercept)"], x=mat[, "x"])
        stan_summary <- capture.output(summary(stan_fit))
        stan_interval <- rstanarm::posterior_interval(stan_fit, prob=input$bayes_prob)
      }
    }
    
    if (is.null(draws)) {
      X <- model.matrix(ols)
      bhat <- coef(ols)
      vbeta <- solve(t(X) %*% X)
      sse <- sum(residuals(ols)^2)
      df_res <- nrow(df) - ncol(X)
      sigma2_draws <- sse / rchisq(input$bayes_iter, df=df_res)
      beta_draws <- t(vapply(sigma2_draws, function(s2) {
        MASS::mvrnorm(1, mu=bhat, Sigma=as.numeric(s2) * vbeta)
      }, numeric(length(bhat))))
      draws <- data.frame(intercept=beta_draws[, 1], x=beta_draws[, 2])
    }
    
    list(df=df, ols=ols, draws=draws, method=method,
         stan_summary=stan_summary, stan_interval=stan_interval)
  }, ignoreInit=FALSE)
  
  output$bayes_preview <- renderDT({
    df <- data()
    if (is.null(df)) return(datatable(data.frame(Message="Load the rocket propellant sample or upload data."), options=list(dom='t')))
    datatable(head(df, 10), options=list(pageLength=5, scrollX=TRUE, dom='tip'), rownames=FALSE)
  })
  
  output$bayes_scatter <- renderPlot({
    df <- bayes_df()
    m <- lm(y ~ x, data=df)
    app_theme()
    plot(df$x, df$y, type="n", xlab=input$bayes_x, ylab=input$bayes_y,
         main="Rocket Propellant: Shear Strength vs Age")
    add_grid()
    points(df$x, df$y, pch=21, bg=adjustcolor(APP_PURPLE,0.55), col=APP_PURPLE, cex=0.9)
    abline(m, col=APP_ORANGE, lwd=2.3)
    legend("topright", legend=c("Observed data","OLS fit"),
           pch=c(21,NA), lty=c(NA,1), col=c(APP_PURPLE,APP_ORANGE),
           pt.bg=c(adjustcolor(APP_PURPLE,0.55),NA), lwd=c(NA,2.3),
           bty="n", cex=0.75, text.col="#374151")
  })
  
  output$bayes_ols_ui <- renderUI({
    bm <- bayes_models()
    s <- summary(bm$ols)
    ci <- confint(bm$ols, level=input$bayes_prob)
    pct <- round(input$bayes_prob * 100)
    HTML(paste0(
      '<div class="result-block">',
      '<div class="rb-equation">OLS: y = beta0 + beta1 x</div>',
      coef_table_html(s, c("(Intercept)", input$bayes_x)),
      '<div class="rb-section">Fit Statistics</div>',
      fit_stats_html(s),
      '<div class="rb-section">', pct, '% Confidence Intervals</div>',
      ci_table_html(ci, c("(Intercept)", input$bayes_x), pct),
      '</div>'))
  })
  
  output$bayes_post_ui <- renderUI({
    bm <- bayes_models()
    draws <- bm$draws
    probs <- c((1-input$bayes_prob)/2, 0.5, 1-(1-input$bayes_prob)/2)
    q_int <- quantile(draws$intercept, probs=probs)
    q_x <- quantile(draws$x, probs=probs)
    rows <- paste0(
      sprintf('<tr><td>(Intercept)</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>',
              fmt_num(mean(draws$intercept)), fmt_num(sd(draws$intercept)),
              fmt_num(q_int[1]), fmt_num(q_int[3])),
      sprintf('<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>',
              input$bayes_x, fmt_num(mean(draws$x)), fmt_num(sd(draws$x)),
              fmt_num(q_x[1]), fmt_num(q_x[3]))
    )
    note <- if (bm$method == "rstanarm::stan_glm")
      "Bayesian model fitted with rstanarm::stan_glm."
    else
      "rstanarm is not installed or Stan fitting failed; using conjugate posterior simulation."
    HTML(paste0(
      '<div class="result-block">',
      '<div class="rb-equation">Bayesian: y = beta0 + beta1 x + error</div>',
      '<div class="rb-section">Method</div>',
      sprintf('<div class="rb-row"><span class="rb-key">%s</span><span class="rb-val">%s draws</span></div>', note, nrow(draws)),
      '<div class="rb-section">Posterior Summaries</div>',
      '<table class="rb-coef-table"><thead><tr><th>Term</th><th>Mean</th><th>SD</th><th>Lower</th><th>Upper</th></tr></thead><tbody>',
      rows, '</tbody></table></div>'))
  })
  
  output$bayes_prob_ui <- renderUI({
    bm <- bayes_models()
    draws <- bm$draws
    p_slope_40 <- mean(draws$x < -40)
    p_slope_band <- mean(draws$x > -38 & draws$x < -36)
    p_int_2600 <- mean(draws$intercept > 2600)
    p_int_band <- mean(draws$intercept > 2400 & draws$intercept < 2700)
    HTML(paste0(
      '<div class="result-block">',
      '<div class="rb-section">Slope beta1</div>',
      sprintf('<div class="rb-row"><span class="rb-key">P(beta1 &lt; -40)</span><span class="rb-val">%s</span></div>', fmt_num(p_slope_40,4)),
      sprintf('<div class="rb-row"><span class="rb-key">P(-38 &lt; beta1 &lt; -36)</span><span class="rb-val">%s</span></div>', fmt_num(p_slope_band,4)),
      '<div class="rb-divider"></div>',
      '<div class="rb-section">Intercept beta0</div>',
      sprintf('<div class="rb-row"><span class="rb-key">P(beta0 &gt; 2600)</span><span class="rb-val">%s</span></div>', fmt_num(p_int_2600,4)),
      sprintf('<div class="rb-row"><span class="rb-key">P(2400 &lt; beta0 &lt; 2700)</span><span class="rb-val">%s</span></div>', fmt_num(p_int_band,4)),
      '</div>'))
  })
  
  output$bayes_slope_plot <- renderPlot({
    bm <- bayes_models()
    app_theme()
    hist(bm$draws$x, breaks=32, freq=FALSE, col="#ede9fe", border="white",
         main="Posterior Distribution of beta1", xlab=paste("Slope for", input$bayes_x))
    add_grid()
    lines(density(bm$draws$x), col=APP_PURPLE, lwd=2.4)
    abline(v=coef(bm$ols)[["x"]], col=APP_ORANGE, lwd=2, lty=2)
    legend("topright", legend=c("Posterior density","OLS slope"),
           col=c(APP_PURPLE,APP_ORANGE), lty=c(1,2), lwd=c(2.4,2),
           bty="n", cex=0.75, text.col="#374151")
  })
  
  glm_models <- eventReactive(input$run_glm, {
    req(data())
    df      <- as.data.frame(data())
    y_col   <- input$glm_y; x_col <- input$glm_x
    type    <- input$glm_type; grp_col <- input$glm_grp
    validate(need(y_col %in% names(df),"Response column not found in data."),
             need(x_col %in% names(df),"Predictor column not found in data."))
    if (type=="binary") {
      df_sub <- df[,c(y_col,x_col),drop=FALSE]
      df_sub <- df_sub[complete.cases(df_sub),]
      validate(need(nrow(df_sub)>=5,"Need at least 5 complete observations."))
      names(df_sub) <- c("y","x")
      validate(need(all(df_sub$y %in% c(0,1)),"Binary response must contain only 0 and 1 values."))
      ols_mod    <- lm(y~x, data=df_sub)
      logit_mod  <- tryCatch(glm(y~x,family=binomial(link="logit"), data=df_sub),error=function(e)NULL)
      probit_mod <- tryCatch(glm(y~x,family=binomial(link="probit"),data=df_sub),error=function(e)NULL)
      list(type="binary",ols=ols_mod,logit=logit_mod,probit=probit_mod,
           df=df_sub,x_col=x_col,y_col=y_col)
    } else {
      has_grp <- !is.null(grp_col) && nchar(grp_col)>0 && grp_col %in% names(df)
      if (has_grp) {
        df_sub <- df[,c(y_col,x_col,grp_col),drop=FALSE]
        df_sub <- df_sub[complete.cases(df_sub),]
        validate(need(nrow(df_sub)>=5,"Need at least 5 complete observations."))
        names(df_sub) <- c("y","x","grp")
        df_sub$y <- as.numeric(df_sub$y); df_sub$x <- as.numeric(df_sub$x)
        df_sub$grp <- factor(df_sub$grp)
        validate(need(all(df_sub$y>=0 & df_sub$y==floor(df_sub$y)),
                      "Poisson response must be non-negative integers."))
        ols_mod   <- lm(y~x,data=df_sub)
        pois_full <- tryCatch(glm(y~grp+x,family="poisson",data=df_sub),error=function(e)NULL)
        pois_red  <- tryCatch(glm(y~x,    family="poisson",data=df_sub),error=function(e)NULL)
        list(type="poisson",has_grp=TRUE,ols=ols_mod,pois_full=pois_full,
             pois_red=pois_red,df=df_sub,x_col=x_col,y_col=y_col,grp_col=grp_col)
      } else {
        df_sub <- df[,c(y_col,x_col),drop=FALSE]
        df_sub <- df_sub[complete.cases(df_sub),]
        validate(need(nrow(df_sub)>=5,"Need at least 5 complete observations."))
        names(df_sub) <- c("y","x")
        df_sub$y <- as.numeric(df_sub$y); df_sub$x <- as.numeric(df_sub$x)
        validate(need(all(df_sub$y>=0 & df_sub$y==floor(df_sub$y)),
                      "Poisson response must be non-negative integers."))
        ols_mod  <- lm(y~x,data=df_sub)
        pois_mod <- tryCatch(glm(y~x,family="poisson",data=df_sub),error=function(e)NULL)
        list(type="poisson",has_grp=FALSE,ols=ols_mod,pois=pois_mod,
             df=df_sub,x_col=x_col,y_col=y_col)
      }
    }
  })
  
  glm_coef_table_html <- function(s) {
    rows <- ""
    for (i in seq_len(nrow(s$coefficients))) {
      est <- s$coefficients[i,1]; se <- s$coefficients[i,2]
      zv  <- s$coefficients[i,3]; pv <- s$coefficients[i,4]
      sc  <- sig_class(pv); sl <- sig_label(pv)
      rows <- paste0(rows,sprintf(
        '<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td><span class="sig-badge %s">%s</span></td></tr>',
        rownames(s$coefficients)[i],fmt_num(est),fmt_num(se),fmt_num(zv,3),sc,sl))
    }
    sprintf('<table class="rb-coef-table"><thead><tr><th>Term</th><th>Estimate</th><th>Std. Error</th><th>z-value</th><th>Significance</th></tr></thead><tbody>%s</tbody></table>',rows)
  }
  
  glm_fit_html <- function(mod) {
    paste0('<div class="rb-fit-grid">',
           sprintf('<div class="rb-fit-cell"><div class="rb-fit-label">Null Deviance</div><div class="rb-fit-value">%s</div></div>',fmt_num(mod$null.deviance,3)),
           sprintf('<div class="rb-fit-cell"><div class="rb-fit-label">Residual Dev.</div><div class="rb-fit-value">%s</div></div>',fmt_num(mod$deviance,3)),
           sprintf('<div class="rb-fit-cell"><div class="rb-fit-label">AIC</div><div class="rb-fit-value">%s</div></div>',fmt_num(mod$aic,3)),
           sprintf('<div class="rb-fit-cell"><div class="rb-fit-label">df (null)</div><div class="rb-fit-value">%d</div></div>',mod$df.null),
           '</div>')
  }
  
  glm_lrt_html <- function(mod) {
    chi_stat <- mod$null.deviance - mod$deviance
    chi_df   <- mod$df.null - mod$df.residual
    chi_p    <- pchisq(chi_stat,chi_df,lower.tail=FALSE)
    paste0('<div class="rb-section">Deviance Test (Overall Significance)</div>',
           sprintf('<div class="rb-test-strip"><span class="rb-test-name">&chi;&sup2; = %s, df = %d</span><span class="rb-test-stat"></span><span class="rb-test-pval">%s</span></div>',
                   fmt_num(chi_stat,4),chi_df,sig_label(chi_p)),
           decision_html(chi_p))
  }
  
  output$glm_binary_plot <- renderPlot({
    m <- glm_models(); req(m$type=="binary")
    df_sub  <- m$df
    x_grid  <- seq(min(df_sub$x,na.rm=TRUE),max(df_sub$x,na.rm=TRUE),length=300)
    nd      <- data.frame(x=x_grid)
    ols_pred    <- predict(m$ols,newdata=nd)
    ols_trunc   <- pmax(0,pmin(1,ols_pred))
    logit_pred  <- if(!is.null(m$logit))  predict(m$logit, newdata=nd,type="response") else rep(NA,length(x_grid))
    probit_pred <- if(!is.null(m$probit)) predict(m$probit,newdata=nd,type="response") else rep(NA,length(x_grid))
    par(mfrow=c(2,2),mar=c(4,4,2.5,1))
    bp <- function(title){plot(df_sub$x,df_sub$y,xlab=m$x_col,ylab=m$y_col,ylim=c(-0.2,1.2),pch=19,col="blue",cex=0.7,main=title,font.main=1,cex.main=1.1)}
    bp("OLS Regression");     lines(x_grid,ols_pred,   col="red",  lwd=2)
    bp("Truncated OLS");      lines(x_grid,ols_trunc,  col="orange",lwd=2)
    bp("Logit Model");        if(!anyNA(logit_pred))  lines(x_grid,logit_pred, col="darkgreen",lwd=2)
    bp("Probit Model");       if(!anyNA(probit_pred)) lines(x_grid,probit_pred,col="steelblue",lwd=2)
    par(mfrow=c(1,1))
  })
  
  output$glm_logit_ui <- renderUI({
    m <- glm_models(); req(m$type=="binary")
    if (is.null(m$logit)) return(div(style="padding:12px;color:#ef4444;font-size:13px;","Logit model failed to converge."))
    HTML(paste0('<div class="result-block"><div class="rb-title">Logit Model Summary</div>',
                '<div class="rb-equation">logit(P(Y=1)) = &beta;<sub>0</sub> + &beta;<sub>1</sub>X</div>',
                glm_coef_table_html(summary(m$logit)),
                '<div class="rb-section">Model Fit</div>',glm_fit_html(m$logit),
                glm_lrt_html(m$logit),'</div>'))
  })
  
  output$glm_probit_ui <- renderUI({
    m <- glm_models(); req(m$type=="binary")
    if (is.null(m$probit)) return(div(style="padding:12px;color:#ef4444;font-size:13px;","Probit model failed to converge."))
    HTML(paste0('<div class="result-block"><div class="rb-title">Probit Model Summary</div>',
                '<div class="rb-equation">&Phi;<sup>-1</sup>(P(Y=1)) = &beta;<sub>0</sub> + &beta;<sub>1</sub>X</div>',
                glm_coef_table_html(summary(m$probit)),
                '<div class="rb-section">Model Fit</div>',glm_fit_html(m$probit),
                glm_lrt_html(m$probit),'</div>'))
  })
  
  output$glm_binary_pred_ui <- renderUI({
    m <- glm_models(); req(m$type=="binary")
    x_str <- input$glm_binary_new_x
    if (is.null(x_str)||!nzchar(trimws(x_str)))
      return(div(style="padding:12px;color:#9ca3af;font-size:13px;","Enter X values above to see predicted probabilities."))
    x_vals <- tryCatch(as.numeric(unlist(strsplit(trimws(x_str),"[,\\s]+"))),error=function(e)NULL)
    if (is.null(x_vals)||anyNA(x_vals))
      return(div(style="padding:12px;color:#ef4444;font-size:13px;","Invalid input \u2014 enter comma-separated numbers."))
    nd      <- data.frame(x=x_vals); x_range <- range(m$df$x,na.rm=TRUE)
    ols_p   <- predict(m$ols,newdata=nd); ols_tr <- pmax(0,pmin(1,ols_p))
    logit_p  <- if(!is.null(m$logit))  predict(m$logit, newdata=nd,type="response") else rep(NA,length(x_vals))
    probit_p <- if(!is.null(m$probit)) predict(m$probit,newdata=nd,type="response") else rep(NA,length(x_vals))
    rows <- ""
    for (i in seq_along(x_vals)) {
      in_rng <- x_vals[i]>=x_range[1] & x_vals[i]<=x_range[2]
      tag    <- if(in_rng)'<span class="interp-tag">interp</span>' else '<span class="extrap-tag">extrap</span>'
      rows   <- paste0(rows,sprintf('<tr><td>%s %s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>',
                                    fmt_num(x_vals[i],3),tag,fmt_num(ols_p[i],4),fmt_num(ols_tr[i],4),
                                    if(!is.na(logit_p[i]))  fmt_num(logit_p[i],4)  else "&mdash;",
                                    if(!is.na(probit_p[i])) fmt_num(probit_p[i],4) else "&mdash;"))
    }
    HTML(paste0('<table class="rb-pred-table"><thead><tr><th>',m$x_col,
                '</th><th>OLS</th><th>OLS (trunc.)</th><th>Logit P\u0302</th><th>Probit P\u0302</th></tr></thead><tbody>',
                rows,'</tbody></table>'))
  })
  
  output$glm_pois_plot <- renderPlot({
    m <- glm_models(); req(m$type=="poisson")
    df_sub <- m$df
    x_grid <- seq(min(df_sub$x,na.rm=TRUE),max(df_sub$x,na.rm=TRUE),length=300)
    nd     <- data.frame(x=x_grid)
    ols_pred  <- predict(m$ols,newdata=nd)
    pois_mod  <- if(m$has_grp) m$pois_red else m$pois
    pois_pred <- if(!is.null(pois_mod)) predict(pois_mod,newdata=nd,type="response") else NULL
    par(mfrow=c(1,2),mar=c(4,4,2.5,1))
    plot(df_sub$x,df_sub$y,xlab=m$x_col,ylab=m$y_col,pch=19,col="blue",cex=0.7,main="OLS Regression",font.main=1,cex.main=1.1)
    lines(x_grid,ols_pred,col="red",lwd=2)
    plot(df_sub$x,df_sub$y,xlab=m$x_col,ylab=m$y_col,pch=19,col="blue",cex=0.7,main="Poisson Regression",font.main=1,cex.main=1.1)
    if(!is.null(pois_pred)) lines(x_grid,pois_pred,col="darkgreen",lwd=2)
    par(mfrow=c(1,1))
  })
  
  output$glm_pois_full_ui <- renderUI({
    m <- glm_models(); req(m$type=="poisson")
    mod <- if(m$has_grp) m$pois_full else m$pois
    if (is.null(mod)) return(div(style="padding:12px;color:#ef4444;font-size:13px;","Poisson model failed."))
    eq <- if(m$has_grp) "log(&lambda;) = &beta;<sub>0</sub> + &beta;<sub>1</sub>X + Group"
    else          "log(&lambda;) = &beta;<sub>0</sub> + &beta;<sub>1</sub>X"
    HTML(paste0('<div class="result-block"><div class="rb-title">Poisson Model Summary</div>',
                '<div class="rb-equation">',eq,'</div>',
                glm_coef_table_html(summary(mod)),
                '<div class="rb-section">Model Fit</div>',glm_fit_html(mod),
                glm_lrt_html(mod),'</div>'))
  })
  
  output$glm_pois_lrt_ui <- renderUI({
    m <- glm_models(); req(m$type=="poisson")
    if (!m$has_grp)
      return(div(style="padding:12px;color:#9ca3af;font-size:13px;",
                 "Select a Group Variable to enable the Likelihood Ratio Test."))
    if (is.null(m$pois_full)||is.null(m$pois_red))
      return(div(style="padding:12px;color:#ef4444;font-size:13px;","Model fitting failed."))
    at       <- anova(m$pois_red,m$pois_full,test="Chisq")
    chi_stat <- at[2,"Deviance"]; chi_df <- at[2,"Df"]; chi_p <- at[2,"Pr(>Chi)"]
    HTML(paste0('<div class="result-block"><div class="rb-title">LRT \u2014 Group Effect</div>',
                '<div class="rb-section">H\u2080: Group has no effect (reduced model)</div>',
                sprintf('<div class="rb-test-strip"><span class="rb-test-name">Full vs Reduced</span><span class="rb-test-stat">&chi;&sup2; = %s, df = %d</span><span class="rb-test-pval">%s</span></div>',
                        fmt_num(chi_stat,4),chi_df,sig_label(chi_p)),
                decision_html(chi_p),
                '<div class="rb-section">Reduced Model (no Group)</div>',
                sprintf('<div class="rb-row"><span class="rb-key">Residual Deviance</span><span class="rb-val">%s</span></div>',fmt_num(m$pois_red$deviance,3)),
                sprintf('<div class="rb-row"><span class="rb-key">AIC</span><span class="rb-val">%s</span></div>',fmt_num(m$pois_red$aic,3)),
                '</div>'))
  })
  
  output$glm_pois_pred_ui <- renderUI({
    m <- glm_models(); req(m$type=="poisson")
    x_str <- input$glm_pois_new_x
    if (is.null(x_str)||!nzchar(trimws(x_str)))
      return(div(style="padding:12px;color:#9ca3af;font-size:13px;","Enter X values above to see predicted counts."))
    x_vals <- tryCatch(as.numeric(unlist(strsplit(trimws(x_str),"[,\\s]+"))),error=function(e)NULL)
    if (is.null(x_vals)||anyNA(x_vals))
      return(div(style="padding:12px;color:#ef4444;font-size:13px;","Invalid input \u2014 enter comma-separated numbers."))
    nd       <- data.frame(x=x_vals); x_range <- range(m$df$x,na.rm=TRUE)
    ols_pred <- predict(m$ols,newdata=nd)
    pois_mod  <- if(m$has_grp) m$pois_red else m$pois
    pois_pred <- if(!is.null(pois_mod)) predict(pois_mod,newdata=nd,type="response") else rep(NA,length(x_vals))
    rows <- ""
    for (i in seq_along(x_vals)) {
      in_rng <- x_vals[i]>=x_range[1] & x_vals[i]<=x_range[2]
      tag    <- if(in_rng)'<span class="interp-tag">interp</span>' else '<span class="extrap-tag">extrap</span>'
      rows   <- paste0(rows,sprintf('<tr><td>%s %s</td><td>%s</td><td>%s</td></tr>',
                                    fmt_num(x_vals[i],3),tag,fmt_num(ols_pred[i],4),
                                    if(!is.na(pois_pred[i])) fmt_num(pois_pred[i],4) else "&mdash;"))
    }
    HTML(paste0('<table class="rb-pred-table"><thead><tr><th>',m$x_col,
                '</th><th>OLS (fitted)</th><th>Poisson \u03bb\u0302</th></tr></thead><tbody>',
                rows,'</tbody></table>'))
  })
  
} # close server

shinyApp(ui, server)
