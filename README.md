# PySGX

Python wrapper for accessing data from SGX's unofficial API

```python

  sgx = SGX()

  stock_code = 'Z74'
  ric = 'STEL.SI'
  ibm_code = '1T75'

  sgx.get_all_stocks()
  # [
  #     {
  #         'issue': 'GTI2013',
  #         'year': 2013,
  #         'companyName':
  #         'SYNEAR FOOD HOLDINGS LIMITED',
  #         'rank': 396,
  #         'adjustment': -2,
  #         'baseScore': 34.0,
  #         'totalScore': 32.0,
  #         'stockCode': 'Z75',
  #         'isinCode': 'BMG8648Q1069'
  #     },
  #     ...
  # ]

  sgx.get_basic_info_by_stock(stock_code)
  # {
  #   "fisn": null,
  #   "ibmCode": "1T75",
  #   "chineseName": "新电信",
  #   "fullName": "SINGTEL",
  #   "stockCode": "Z74",
  #   "isinCode": "SG1T75931496"
  # }

  sgx.get_historical_data(stock_code)
  # [{
  #     'change_vs_pc_percentage': None,
  #     'cur': 'SGD',
  #     'ptd': '20190718',
  #     'pv': 3.52,
  #     'h': 3.54,
  #     'lt': 3.54,
  #     'trading_time': '20190719_091600',
  #     'l': 3.52,
  #     'type': 'stocks',
  #     'dp': None,
  #     'n': 'SingTel',
  #     'o': 3.54,
  #     'change_vs_pc': None,
  #     'du': None,
  #     'nc': 'Z74',
  #     'v': 47160653.0,
  #     'vl': 13353.0,
  #     'dpc': None,
  #     'lf': None,
  #     'ig': None,
  #     'ed': None
  #     },
  # ...]

  sgx.get_stock_announcements(stock_code)
  # [
  #   {
  #     "ref_id": "SG180725OTHRHGS2",
  #     "sub": "ANNC17",
  #     "category_name": "Financial Statements",
  #     "submitted_by": "Lim Li Ching (Ms)",
  #     "title": "Financial Statements and Related Announcement::Notification of Results Release",
  #     "announcer_name": null,
  #     "issuers": [
  #       {
  #         "isin_code": "SG1T75931496",
  #         "stock_code": "Z74",
  #         "security_name": "SINGTEL",
  #         "issuer_name": "SINGAPORE TELECOMMUNICATIONS LIMITED",
  #         "ibm_code": "1T75"
  #       }
  #     ],
  #     "security_name": "SINGTEL",
  #     "url": "https://links.sgx.com/1.0.0/corporate-announcements/RJ4CD5XI53N69BAA/11714ceb6c1d5f2143700b647b8d271e81202829ff4c934816306a739db8cde8",
  #     "issuer_name": "SINGAPORE TELECOMMUNICATIONS LIMITED",
  #     "submission_date": "20180725",
  #     "submission_date_time": 1532510053000,
  #     "broadcast_date_time": 1532510053000,
  #     "xml": null,
  #     "submission_time": null,
  #     "cat": "ANNC",
  #     "id": "RJ4CD5XI53N69BAA",
  #     "sn": null,
  #     "product_category": null
  #   },
  #   ...
  # ]

  sgx.get_general_information(ric)
  # {
  #   "company_name": "Singapore Telecommunications Limited",
  #   "industry_classification": [
  #     "Integrated Telecommunications Services",
  #     "Integrated Telecommunications Services - NEC",
  #     "Integrated Telecommunications Services",
  #     "Cellular and Other Wireless Telecommunications",
  #     "Wired Telecommunications Carriers",
  #     "On-Line Information Services",
  #     "Cable and Other Program Distribution",
  #     "Other Electronic Parts and Equipment Wholesalers",
  #     "Engineering Services",
  #     "Offices of  Other Holding Companies",
  #     "Radiotelephone Communications",
  #     "Telephon Communications Not Radio",
  #     "Information Retrieval Services",
  #     "Cable And Other Pay Tv Services",
  #     "Electronic Parts And Equipment",
  #     "Engineering Services",
  #     "Holding Companies, Nec",
  #     "Services",
  #     "Communications Services"
  #   ],
  #   "employees": "24071"
  # }

  sgx.get_ratios(ric)
  # {
  #   "issues": {
  #     "name": "Common Stock",
  #     "Name": "Fully Paid Ord. Shrs",
  #     "Ticker": "Z74",
  #     "RIC": "STEL.SI",
  #     "DisplayRIC": "STEL.SI",
  #     "InstrumentPI": "369495",
  #     "QuotePI": "1077054",
  #     "exchange": {
  #       "code": "SIN",
  #       "country": "SGP",
  #       "name": "Singapore Exchange Securities Trading"
  #     },
  #     "mostRecentSplit": {
  #       "date": "2004-09-23",
  #       "value": "0.92857"
  #     }
  #   },
  #   "generalInfo": {
  #     "companyStatus": "Active",
  #     "companyType": "Equity Issue",
  #     "lastModified": "2020-05-28",
  #     "latestAvailableAnnual": "2020-03-31",
  #     "latestAvailableInterim": "2020-03-31",
  #     "employees": {
  #       "lastUpdated": "2019-03-31",
  #       "value": "24071"
  #     },
  #     "sharesOut": {
  #       "date": "2020-06-01",
  #       "totalFloat": "15478990807.0",
  #       "value": "16320676286.0"
  #     },
  #     "reportingCurrency": {
  #       "code": "SGD",
  #       "value": "Singapore Dollars"
  #     }
  #   },
  #   "ratios": {
  #     "priceCurrency": "SGD",
  #     "reportingCurrency": "SGD",
  #     "exchangeRate": "1.00000",
  #     "date": "2020-03-31",
  #     "Price and Volume": {
  #       "NPRICE": "2.49000",
  #       "NHIG": "3.56000",
  #       "NLOW": "2.19000",
  #       "PDATE": "2020-06-26T00:00:00",
  #       "VOL10DAVG": "35.74858",
  #       "MKTCAP": "40659.61000",
  #       "NHIGDATE": "2019-07-05T00:00:00",
  #       "NLOWDATE": "2020-03-23T00:00:00",
  #       "VOL3MAVG": "758.98260",
  #       "BETA": "0.84113",
  #       "PR1DAYPRC": "0.40323",
  #       "PR13WKPCT": "-3.11284",
  #       "PR26WKPCT": "-26.11276",
  #       "PR5DAYPRC": "-1.19048",
  #       "PR52WKPCT": "-28.44828",
  #       "PRYTDPCT": "-26.11276",
  #       "ChPctPriceMTD": "0.00000",
  #       "PR04WKPCTR": "-3.59991",
  #       "PR13WKPCTR": "-5.93072",
  #       "PR26WKPCTR": "-8.46670",
  #       "PR52WKPCTR": "-8.55590",
  #       "PRYTDPCTR": "-8.57166"
  #     },
  #     "Per share data": {
  #       "AEPSXCLXOR": "0.06573",
  #       "TTMEPSXCLX": "0.06572",
  #       "AEPSNORM": "0.04849",
  #       "AREVPS": "1.01181",
  #       "TTMREVPS": "1.01255",
  #       "ABVPS": "1.64143",
  #       "QBVPS": "1.64143",
  #       "ATANBVPS": "0.79980",
  #       "QTANBVPS": "0.79980",
  #       "ACSHPS": "0.06125",
  #       "QCSHPS": "0.06125",
  #       "ACFSHR": "0.22218",
  #       "TTMCFSHR": "0.22234",
  #       "ADIVSHR": "0.12250",
  #       "TTMDIVSHR": "0.12250",
  #       "TTMEBITDPS": "0.27803",
  #       "ABEPSXCLXO": "0.06584",
  #       "TTMBEPSXCL": "0.06581",
  #       "AEPSINCLXO": "0.06573",
  #       "TTMEPSINCX": "0.06572",
  #       "TTMFCFSHR": "0.14205",
  #       "ADIV5YAVG": "0.16450"
  #     },
  #     "Valuation": {
  #       "APEEXCLXOR": "37.88225",
  #       "PEEXCLXOR": "37.88801",
  #       "APENORM": "51.35079",
  #       "APR2REV": "2.45792",
  #       "TTMPR2REV": "2.45792",
  #       "APR2TANBK": "3.11489",
  #       "PR2TANBK": "3.11489",
  #       "APRFCFPS": "70.83555",
  #       "TTMPRCFPS": "11.19328",
  #       "TTMPRFCFPS": "17.52041",
  #       "APRICE2BK": "1.51697",
  #       "PRICE2BK": "1.51697",
  #       "PEBEXCLXOR": "38.59596",
  #       "TTMPEHIGH": "43.26614",
  #       "TTMPELOW": "9.29811",
  #       "PEINCLXOR": "37.88801",
  #       "NetDebt_I": "13173.00000",
  #       "NetDebt_A": "13173.00000",
  #       "YLD5YAVG": "4.95241",
  #       "YIELD": "4.91968",
  #       "DivYield_CurTTM": "4.91968"
  #     },
  #     "Financial strength": {
  #       "ACURRATIO": "0.67834",
  #       "QCURRATIO": "0.67834",
  #       "AQUICKRATI": "0.65191",
  #       "QQUICKRATI": "0.65191",
  #       "ALTD2EQ": "38.08288",
  #       "QLTD2EQ": "38.08288",
  #       "ATOTD2EQ": "52.90416",
  #       "QTOTD2EQ": "52.90416",
  #       "APAYRATIO": "186.04840",
  #       "TTMPAYRAT": "0.00000",
  #       "EV2FCF_CurA": "-99999.99000",
  #       "EV2FCF_CurTTM": "396.30170",
  #       "AINTCOV": "2.40805",
  #       "TTMINTCOV": "2.46482",
  #       "A1FCF": "574.00000",
  #       "TTMFCF": "2320.70000"
  #     },
  #     "Income Statement": {
  #       "AREV": "16542.30000",
  #       "TTMREV": "16542.30000",
  #       "AEBITD": "4537.80000",
  #       "TTMEBITD": "4542.30000",
  #       "AEBT": "1565.40000",
  #       "TTMEBT": "1565.40000",
  #       "ANIAC": "1074.60000",
  #       "TTMNIAC": "1074.60000",
  #       "AEBTNORM": "1146.10000",
  #       "ANIACNORM": "792.76310",
  #       "VDES_TTM": "0.04436"
  #     },
  #     "Margins": {
  #       "AGROSMGN": "-99999.99000",
  #       "TTMGROSMGN": "76.38116",
  #       "ANPMGNPCT": "6.36066",
  #       "TTMNPMGN": "6.36066",
  #       "AOPMGNPCT": "14.36801",
  #       "TTMOPMGN": "14.36801",
  #       "TTMPTMGN": "9.46301",
  #       "APTMGNPCT": "9.46301",
  #       "OPMGN5YR": "17.89767",
  #       "PTMGN5YR": "24.23251",
  #       "Focf2Rev_AAvg5": "-0.11938",
  #       "Focf2Rev_TTM": "14.02888",
  #       "GROSMGN5YR": "-99999.99000",
  #       "MARGIN5YR": "20.34598"
  #     },
  #     "Mgmt Effectiveness": {
  #       "AROAPCT": "2.15021",
  #       "TTMROAPCT": "2.15021",
  #       "AROEPCT": "3.79536",
  #       "TTMROEPCT": "3.79536",
  #       "AROIPCT": "2.68076",
  #       "TTMROIPCT": "2.68076",
  #       "AROA5YAVG": "7.34794",
  #       "AROE5YAVG": "12.53840",
  #       "AROI5YRAVG": "8.92042",
  #       "AASTTURN": "0.33805",
  #       "TTMASTTURN": "0.33805",
  #       "AINVTURN": "-99999.99000",
  #       "TTMINVTURN": "0.02266",
  #       "ANIPEREMP": "-99999.99000",
  #       "TTMNIPEREM": "-99999.99000",
  #       "ARECTURN": "3.22390",
  #       "TTMRECTURN": "2.86395",
  #       "AREVPEREMP": "-99999.99000",
  #       "TTMREVPERE": "-99999.99000"
  #     },
  #     "Growth": {
  #       "REVCHNGYR": "-10.19807",
  #       "REVTRENDGR": "-0.80314",
  #       "EPSCHNGYR": "-25.72395",
  #       "TTMEPSCHG": "-65.28629",
  #       "EPSTRENDGR": "-22.60366",
  #       "DIVGRPCT": "-11.20959",
  #       "TTMREVCHG": "-4.77443",
  #       "REVPS5YGR": "-1.25899",
  #       "REVGRPCT": "-0.33844",
  #       "EPSGRPCT": "-34.98326",
  #       "BVTRENDGR": "1.13351",
  #       "TanBV_AYr5CAGR": "0.41670",
  #       "CSPTRENDGR": "-5.71811",
  #       "Ebitda_AYr5CAGR": "-2.27706",
  #       "Ebitda_TTMY5CAGR": "-2.25769",
  #       "FOCF_AYr5CAGR": "-99999.99000",
  #       "STLD_AYr5CAGR": "9.55854",
  #       "NPMTRENDGR": "-21.95950"
  #     }
  #   },
  #   "forecastData": {
  #     "consensusType": "Mean",
  #     "currentFiscalYear": "2021",
  #     "currentFiscalYearEndMonth": "3",
  #     "ProjSales": "15789.15240",
  #     "ProjSalesH": "17392.00000",
  #     "ProjSalesL": "15049.55600",
  #     "ProjSalesNumOfEst": "17",
  #     "ProjSalesPS": "0.96693",
  #     "ProjSalesQ": "3623.41000",
  #     "ProjSalesQH": "3623.41000",
  #     "ProjSalesQL": "3623.41000",
  #     "ProjSalesQNumOfEst": "1",
  #     "Price2ProjSales": "2.57516",
  #     "ProjEPS": "0.15682",
  #     "ProjEPSH": "0.20700",
  #     "ProjEPSL": "0.13000",
  #     "ProjEPSNumOfEst": "17",
  #     "ProjEPSQ": "0.04000",
  #     "ProjEPSQH": "0.04000",
  #     "ProjEPSQL": "0.04000",
  #     "ProjEPSQNumOfEst": "3",
  #     "ProjPE": "15.87808",
  #     "ProjLTGrowthRate": "-99999.99000",
  #     "TargetPrice": "3.21072",
  #     "EPSActual": "0.15000",
  #     "EPSPrev": "0.14850",
  #     "EPSSurprise": "0.00150",
  #     "EPSSurprisePrc": "1.01",
  #     "EPSActualQ": "0.03600",
  #     "EPSPrevQ": "0.07425",
  #     "EPSSurpriseQ": "-0.03825",
  #     "EPSSurpriseQPrc": "-51.52",
  #     "ProjProfit": "3337.40870",
  #     "ProjProfitH": "4447.07000",
  #     "ProjProfitL": "2697.00000",
  #     "ProjProfitNumOfEst": "17",
  #     "ProjDPS": "0.12256",
  #     "ProjDPSH": "0.15000",
  #     "ProjDPSL": "0.08600",
  #     "ProjDPSNumOfEst": "16"
  #   },
  #   "recommendation": {
  #     "opinions": {
  #       "BUY": {
  #         "CURR": "5",
  #         "1WA": "5",
  #         "1MA": "5",
  #         "2MA": "5",
  #         "3MA": "4"
  #       },
  #       "OUTPERFORM": {
  #         "CURR": "9",
  #         "1WA": "9",
  #         "1MA": "9",
  #         "2MA": "9",
  #         "3MA": "8"
  #       },
  #       "HOLD": {
  #         "CURR": "3",
  #         "1WA": "3",
  #         "1MA": "3",
  #         "2MA": "3",
  #         "3MA": "5"
  #       },
  #       "UNDERPERFORM": {
  #         "CURR": "1",
  #         "1WA": "1",
  #         "1MA": "1",
  #         "2MA": "1",
  #         "3MA": "1"
  #       },
  #       "SELL": {
  #         "CURR": "0",
  #         "1WA": "0",
  #         "1MA": "0",
  #         "2MA": "0",
  #         "3MA": "0"
  #       },
  #       "NA": {
  #         "CURR": "0",
  #         "1WA": "0",
  #         "1MA": "0",
  #         "2MA": "0",
  #         "3MA": "0"
  #       }
  #     },
  #     "meanRating": {
  #       "CURR": "2",
  #       "1WA": "2",
  #       "1MA": "2",
  #       "2MA": "2",
  #       "3MA": "2.1667"
  #     },
  #     "noOfAnalysts": {
  #       "CURR": "18",
  #       "1WA": "18",
  #       "1MA": "18",
  #       "2MA": "18",
  #       "3MA": "18"
  #     }
  #   }
  # }

  sgx.get_corporate_info(ric)
  # {
  #   "country": "SINGAPORE",
  #   "incorporatedOn": "19920328",
  #   "webAddress": "http://www.singtel.com",
  #   "companyName": "SINGTEL",
  #   "ibmCode": "1T75",
  #   "listedDate": "1 November 1993 on SGX Mainboard",
  #   "registeredOffice1": "31 Exeter Road",
  #   "registeredOffice3": "Singapore 239732",
  #   "registeredOffice2": "Comcentre",
  #   "registeredOffice4": "     ",
  #   "market": "MAINBOARD",
  #   "countryCode": "SGP       ",
  #   "tradingCurrency": "SGD",
  #   "background": "Singapore Telecommunications Limited (Singtel) was corporatised on 1 April 1992 and listed on the local stock exchange in November 1993. It is majority-owned (2019: 49.8 percent) by Temasek Holdings (Private) Limited.\nSingtel is a leading communications technology group in Asia, and has played a key role in Singapores development as a telecommunications hub for the region over the course of its 140-year history. Headquartered in Singapore, Singtel provides an extensive range of telecommunications and digital services to consumers and enterprises through its three business groups - Group Consumer, Group Enterprise and Group Digital Life. \nSingtel has stakes in leading mobile operators in high-growth emerging markets - AIS in Thailand, Bharti Airtel in India, Globe in the Philippines and Telkomsel in Indonesia. Together with these regional associates and its wholly-owned subsidiary Optus, Singtel serves over 700 million mobile customers in 21 countries. Its infrastructure and technology services for businesses span 21 countries with more than 428 direct points of presence in 362 cities.",
  #   "id": 2032
  # }

  sgx.get_corporate_actions(ibm_code)
  # [
  #   {
  #     "url": "https://links.sgx.com/1.0.0/corporate-actions/27098",
  #     "datePaid": 1471363200000,
  #     "anncType": "DIVIDEND",
  #     "exDate": 1470067200000,
  #     "name": "SINGTEL",
  #     "id": 27098,
  #     "particulars": "SGD 0.107 ONE-TIER TAX",
  #     "recDate": 1470240000000
  #   },
  #   ...
  # ]

  sgx.get_financial_statements(ric)
  # {
  #   "RepNo": "A35C6",
  #   "CompanyName": "Singapore Telecommunications Limited",
  #   "lineItemTypes": {
  #     "SREV": "Revenue",
  #     "SORE": "Other Revenue, Total",
  #     "RTLR": "Total Revenue",
  #     "SCOR": "Cost of Revenue, Total",
  #     "SGRP": "Gross Profit",
  #     "SSGA": "Selling/General/Admin. Expenses, Total",
  #     "ERAD": "Research & Development",
  #     "SDPR": "Depreciation/Amortization",
  #     "SINN": "Interest Exp.(Inc.),Net-Operating, Total",
  #     "SUIE": "Unusual Expense (Income)",
  #     "SOOE": "Other Operating Expenses, Total",
  #     "ETOE": "Total Operating Expense",
  #     "SOPI": "Operating Income",
  #     "SNIN": "Interest Inc.(Exp.),Net-Non-Op., Total",
  #     "NGLA": "Gain (Loss) on Sale of Assets",
  #     "SONT": "Other, Net",
  #     "EIBT": "Net Income Before Taxes",
  #     "TTAX": "Provision for Income Taxes",
  #     "TIAT": "Net Income After Taxes",
  #     "CMIN": "Minority Interest",
  #     "CEIA": "Equity In Affiliates",
  #     "CGAP": "U.S. GAAP Adjustment",
  #     "NIBX": "Net Income Before Extra. Items",
  #     "STXI": "Total Extraordinary Items",
  #     "NINC": "Net Income",
  #     "SANI": "Total Adjustments to Net Income",
  #     "CIAC": "Income Available to Com Excl ExtraOrd",
  #     "XNIC": "Income Available to Com Incl ExtraOrd",
  #     "SDAJ": "Dilution Adjustment",
  #     "SDNI": "Diluted Net Income",
  #     "SDWS": "Diluted Weighted Average Shares",
  #     "SDBF": "Diluted EPS Excluding ExtraOrd Items",
  #     "DDPS1": "DPS - Common Stock Primary Issue",
  #     "VDES": "Diluted Normalized EPS",
  #     "ACSH": "Cash",
  #     "ACAE": "Cash & Equivalents",
  #     "ASTI": "Short Term Investments",
  #     "SCSI": "Cash and Short Term Investments",
  #     "AACR": "Accounts Receivable - Trade, Net",
  #     "ATRC": "Total Receivables, Net",
  #     "AITL": "Total Inventory",
  #     "APPY": "Prepaid Expenses",
  #     "SOCA": "Other Current Assets, Total",
  #     "ATCA": "Total Current Assets",
  #     "APTC": "Property/Plant/Equipment, Total - Gross",
  #     "ADEP": "Accumulated Depreciation, Total",
  #     "APPN": "Property/Plant/Equipment, Total - Net",
  #     "AGWI": "Goodwill, Net",
  #     "AINT": "Intangibles, Net",
  #     "SINV": "Long Term Investments",
  #     "ALTR": "Note Receivable - Long Term",
  #     "SOLA": "Other Long Term Assets, Total",
  #     "ATOT": "Total Assets",
  #     "LAPB": "Accounts Payable",
  #     "LPBA": "Payable/Accrued",
  #     "LAEX": "Accrued Expenses",
  #     "LSTD": "Notes Payable/Short Term Debt",
  #     "LCLD": "Current Port. of  LT Debt/Capital Leases",
  #     "SOCL": "Other Current liabilities, Total",
  #     "LTCL": "Total Current Liabilities",
  #     "LLTD": "Long Term Debt",
  #     "LCLO": "Capital Lease Obligations",
  #     "LTTD": "Total Long Term Debt",
  #     "STLD": "Total Debt",
  #     "SBDT": "Deferred Income Tax",
  #     "LMIN": "Minority Interest",
  #     "SLTL": "Other Liabilities, Total",
  #     "LTLL": "Total Liabilities",
  #     "SRPR": "Redeemable Preferred Stock, Total",
  #     "SPRS": "Preferred Stock - Non Redeemable, Net",
  #     "SCMS": "Common Stock, Total",
  #     "QPIC": "Additional Paid-In Capital",
  #     "QRED": "Retained Earnings (Accumulated Deficit)",
  #     "QTSC": "Treasury Stock - Common",
  #     "QEDG": "ESOP Debt Guarantee",
  #     "QUGL": "Unrealized Gain (Loss)",
  #     "SOTE": "Other Equity, Total",
  #     "QTLE": "Total Equity",
  #     "QTEL": "Total Liabilities & Shareholders' Equity",
  #     "QTCO": "Total Common Shares Outstanding",
  #     "QTPO": "Total Preferred Shares Outstanding",
  #     "STBP": "Tangible Book Value per Share, Common Eq",
  #     "ONET": "Net Income/Starting Line",
  #     "SDED": "Depreciation/Depletion",
  #     "SAMT": "Amortization",
  #     "OBDT": "Deferred Taxes",
  #     "SNCI": "Non-Cash Items",
  #     "SOCF": "Changes in Working Capital",
  #     "OTLO": "Cash from Operating Activities",
  #     "SCEX": "Capital Expenditures",
  #     "SICF": "Other Investing Cash Flow Items, Total",
  #     "ITLI": "Cash from Investing Activities",
  #     "SFCF": "Financing Cash Flow Items",
  #     "FCDP": "Total Cash Dividends Paid",
  #     "FPSS": "Issuance (Retirement) of Stock, Net",
  #     "FPRD": "Issuance (Retirement) of Debt, Net",
  #     "FTLF": "Cash from Financing Activities",
  #     "SFEE": "Foreign Exchange Effects",
  #     "SNCC": "Net Change in Cash",
  #     "SCIP": "Cash Interest Paid",
  #     "SCTP": "Cash Taxes Paid"
  #   },
  #   "statements": [
  #     {
  #       "type": "Annual",
  #       "endDate": "2020-03-31",
  #       "fiscalYear": "2020",
  #       "income": {
  #         "SREV": {
  #           "name": "Revenue",
  #           "value": "16542.300000"
  #         },
  #         "RTLR": {
  #           "name": "Total Revenue",
  #           "value": "16542.300000"
  #         },
  #         "SDPR": {
  #           "name": "Depreciation/Amortization",
  #           "value": "2580.300000"
  #         },
  #         "SUIE": {
  #           "name": "Unusual Expense (Income)",
  #           "value": "-419.300000"
  #         },
  #         "SOOE": {
  #           "name": "Other Operating Expenses, Total",
  #           "value": "12004.500000"
  #         },
  #         "ETOE": {
  #           "name": "Total Operating Expense",
  #           "value": "14165.500000"
  #         },
  #         "SOPI": {
  #           "name": "Operating Income",
  #           "value": "2376.800000"
  #         },
  #         "SNIN": {
  #           "name": "Interest Inc.(Exp.),Net-Non-Op., Total",
  #           "value": "-812.900000"
  #         },
  #         "SONT": {
  #           "name": "Other, Net",
  #           "value": "1.500000"
  #         },
  #         "EIBT": {
  #           "name": "Net Income Before Taxes",
  #           "value": "1565.400000"
  #         },
  #         "TTAX": {
  #           "name": "Provision for Income Taxes",
  #           "value": "513.200000"
  #         },
  #         "TIAT": {
  #           "name": "Net Income After Taxes",
  #           "value": "1052.200000"
  #         },
  #         "CMIN": {
  #           "name": "Minority Interest",
  #           "value": "22.400000"
  #         },
  #         "NIBX": {
  #           "name": "Net Income Before Extra. Items",
  #           "value": "1074.600000"
  #         },
  #         "NINC": {
  #           "name": "Net Income",
  #           "value": "1074.600000"
  #         },
  #         "CIAC": {
  #           "name": "Income Available to Com Excl ExtraOrd",
  #           "value": "1074.600000"
  #         },
  #         "XNIC": {
  #           "name": "Income Available to Com Incl ExtraOrd",
  #           "value": "1074.600000"
  #         },
  #         "SDNI": {
  #           "name": "Diluted Net Income",
  #           "value": "1074.600000"
  #         },
  #         "SDWS": {
  #           "name": "Diluted Weighted Average Shares",
  #           "value": "16349.228000"
  #         },
  #         "SDBF": {
  #           "name": "Diluted EPS Excluding ExtraOrd Items",
  #           "value": "0.065730"
  #         },
  #         "DDPS1": {
  #           "name": "DPS - Common Stock Primary Issue",
  #           "value": "0.122500"
  #         },
  #         "VDES": {
  #           "name": "Diluted Normalized EPS",
  #           "value": "0.048490"
  #         }
  #       },
  #       "balance": {
  #         "ACAE": {
  #           "name": "Cash & Equivalents",
  #           "value": "999.600000"
  #         },
  #         "SCSI": {
  #           "name": "Cash and Short Term Investments",
  #           "value": "999.600000"
  #         },
  #         "AACR": {
  #           "name": "Accounts Receivable - Trade, Net",
  #           "value": "5559.400000"
  #         },
  #         "ATRC": {
  #           "name": "Total Receivables, Net",
  #           "value": "5559.400000"
  #         },
  #         "AITL": {
  #           "name": "Total Inventory",
  #           "value": "279.600000"
  #         },
  #         "SOCA": {
  #           "name": "Other Current Assets, Total",
  #           "value": "337.200000"
  #         },
  #         "ATCA": {
  #           "name": "Total Current Assets",
  #           "value": "7175.800000"
  #         },
  #         "APPN": {
  #           "name": "Property/Plant/Equipment, Total - Net",
  #           "value": "12424.300000"
  #         },
  #         "AINT": {
  #           "name": "Intangibles, Net",
  #           "value": "13735.900000"
  #         },
  #         "SINV": {
  #           "name": "Long Term Investments",
  #           "value": "14226.800000"
  #         },
  #         "SOLA": {
  #           "name": "Other Long Term Assets, Total",
  #           "value": "1392.100000"
  #         },
  #         "ATOT": {
  #           "name": "Total Assets",
  #           "value": "48954.900000"
  #         },
  #         "LPBA": {
  #           "name": "Payable/Accrued",
  #           "value": "5640.900000"
  #         },
  #         "LCLD": {
  #           "name": "Current Port. of  LT Debt/Capital Leases",
  #           "value": "3970.500000"
  #         },
  #         "SOCL": {
  #           "name": "Other Current liabilities, Total",
  #           "value": "967.100000"
  #         },
  #         "LTCL": {
  #           "name": "Total Current Liabilities",
  #           "value": "10578.500000"
  #         },
  #         "LLTD": {
  #           "name": "Long Term Debt",
  #           "value": "10202.100000"
  #         },
  #         "LTTD": {
  #           "name": "Total Long Term Debt",
  #           "value": "10202.100000"
  #         },
  #         "STLD": {
  #           "name": "Total Debt",
  #           "value": "14172.600000"
  #         },
  #         "SBDT": {
  #           "name": "Deferred Income Tax",
  #           "value": "525.500000"
  #         },
  #         "LMIN": {
  #           "name": "Minority Interest",
  #           "value": "24.800000"
  #         },
  #         "SLTL": {
  #           "name": "Other Liabilities, Total",
  #           "value": "834.800000"
  #         },
  #         "LTLL": {
  #           "name": "Total Liabilities",
  #           "value": "22165.700000"
  #         },
  #         "SCMS": {
  #           "name": "Common Stock, Total",
  #           "value": "4127.300000"
  #         },
  #         "QRED": {
  #           "name": "Retained Earnings (Accumulated Deficit)",
  #           "value": "22661.900000"
  #         },
  #         "QTLE": {
  #           "name": "Total Equity",
  #           "value": "26789.200000"
  #         },
  #         "QTEL": {
  #           "name": "Total Liabilities & Shareholders' Equity",
  #           "value": "48954.900000"
  #         },
  #         "QTCO": {
  #           "name": "Total Common Shares Outstanding",
  #           "value": "16320.617990"
  #         },
  #         "STBP": {
  #           "name": "Tangible Book Value per Share, Common Eq",
  #           "value": "0.799800"
  #         }
  #       },
  #       "cash": {
  #         "ONET": {
  #           "name": "Net Income/Starting Line",
  #           "value": "1565.400000"
  #         },
  #         "SDED": {
  #           "name": "Depreciation/Depletion",
  #           "value": "2580.300000"
  #         },
  #         "SNCI": {
  #           "name": "Non-Cash Items",
  #           "value": "361.000000"
  #         },
  #         "SCTP": {
  #           "name": "Cash Taxes Paid",
  #           "value": "491.900000"
  #         },
  #         "SCIP": {
  #           "name": "Cash Interest Paid",
  #           "value": "463.300000"
  #         },
  #         "SOCF": {
  #           "name": "Changes in Working Capital",
  #           "value": "1310.600000"
  #         },
  #         "OTLO": {
  #           "name": "Cash from Operating Activities",
  #           "value": "5817.300000"
  #         },
  #         "SCEX": {
  #           "name": "Capital Expenditures",
  #           "value": "-2386.600000"
  #         },
  #         "SICF": {
  #           "name": "Other Investing Cash Flow Items, Total",
  #           "value": "-534.200000"
  #         },
  #         "ITLI": {
  #           "name": "Cash from Investing Activities",
  #           "value": "-2920.800000"
  #         },
  #         "SFCF": {
  #           "name": "Financing Cash Flow Items",
  #           "value": "-293.300000"
  #         },
  #         "FCDP": {
  #           "name": "Total Cash Dividends Paid",
  #           "value": "-2856.700000"
  #         },
  #         "FPSS": {
  #           "name": "Issuance (Retirement) of Stock, Net",
  #           "value": "-23.000000"
  #         },
  #         "FPRD": {
  #           "name": "Issuance (Retirement) of Debt, Net",
  #           "value": "726.200000"
  #         },
  #         "FTLF": {
  #           "name": "Cash from Financing Activities",
  #           "value": "-2446.800000"
  #         },
  #         "SFEE": {
  #           "name": "Foreign Exchange Effects",
  #           "value": "37.200000"
  #         },
  #         "SNCC": {
  #           "name": "Net Change in Cash",
  #           "value": "486.900000"
  #         }
  #       }
  #     },
  #     {
  #       "type": "Annual",
  #       "endDate": "2019-03-31",
  #       "fiscalYear": "2019",
  #       "income": {
  #         "SREV": {
  #           "name": "Revenue",
  #           "value": "17371.700000"
  #         },
  #         "RTLR": {
  #           "name": "Total Revenue",
  #           "value": "17371.700000"
  #         },
  #         "SCOR": {
  #           "name": "Cost of Revenue, Total",
  #           "value": "7834.600000"
  #         },
  #         "SGRP": {
  #           "name": "Gross Profit",
  #           "value": "9537.100000"
  #         },
  #         "SSGA": {
  #           "name": "Selling/General/Admin. Expenses, Total",
  #           "value": "5069.900000"
  #         },
  #         "SDPR": {
  #           "name": "Depreciation/Amortization",
  #           "value": "2222.200000"
  #         },
  #         "SINN": {
  #           "name": "Interest Exp.(Inc.),Net-Operating, Total",
  #           "value": "-3.400000"
  #         },
  #         "SUIE": {
  #           "name": "Unusual Expense (Income)",
  #           "value": "-73.500000"
  #         },
  #         "SOOE": {
  #           "name": "Other Operating Expenses, Total",
  #           "value": "-216.000000"
  #         },
  #         "ETOE": {
  #           "name": "Total Operating Expense",
  #           "value": "14833.800000"
  #         },
  #         "SOPI": {
  #           "name": "Operating Income",
  #           "value": "2537.900000"
  #         },
  #         "SNIN": {
  #           "name": "Interest Inc.(Exp.),Net-Non-Op., Total",
  #           "value": "1214.700000"
  #         },
  #         "SONT": {
  #           "name": "Other, Net",
  #           "value": "-6.700000"
  #         },
  #         "EIBT": {
  #           "name": "Net Income Before Taxes",
  #           "value": "3745.900000"
  #         },
  #         "TTAX": {
  #           "name": "Provision for Income Taxes",
  #           "value": "674.800000"
  #         },
  #         "TIAT": {
  #           "name": "Net Income After Taxes",
  #           "value": "3071.100000"
  #         },
  #         "CMIN": {
  #           "name": "Minority Interest",
  #           "value": "23.400000"
  #         },
  #         "NIBX": {
  #           "name": "Net Income Before Extra. Items",
  #           "value": "3094.500000"
  #         },
  #         "NINC": {
  #           "name": "Net Income",
  #           "value": "3094.500000"
  #         },
  #         "CIAC": {
  #           "name": "Income Available to Com Excl ExtraOrd",
  #           "value": "3094.500000"
  #         },
  #         "XNIC": {
  #           "name": "Income Available to Com Incl ExtraOrd",
  #           "value": "3094.500000"
  #         },
  #         "SDNI": {
  #           "name": "Diluted Net Income",
  #           "value": "3094.500000"
  #         },
  #         "SDWS": {
  #           "name": "Diluted Weighted Average Shares",
  #           "value": "16342.302000"
  #         },
  #         "SDBF": {
  #           "name": "Diluted EPS Excluding ExtraOrd Items",
  #           "value": "0.189360"
  #         },
  #         "DDPS1": {
  #           "name": "DPS - Common Stock Primary Issue",
  #           "value": "0.175000"
  #         },
  #         "VDES": {
  #           "name": "Diluted Normalized EPS",
  #           "value": "0.185670"
  #         }
  #       },
  #       "balance": {
  #         "ACSH": {
  #           "name": "Cash",
  #           "value": "359.200000"
  #         },
  #         "ACAE": {
  #           "name": "Cash & Equivalents",
  #           "value": "142.900000"
  #         },
  #         "ASTI": {
  #           "name": "Short Term Investments",
  #           "value": "10.600000"
  #         },
  #         "SCSI": {
  #           "name": "Cash and Short Term Investments",
  #           "value": "512.700000"
  #         },
  #         "AACR": {
  #           "name": "Accounts Receivable - Trade, Net",
  #           "value": "4702.900000"
  #         },
  #         "ATRC": {
  #           "name": "Total Receivables, Net",
  #           "value": "5307.700000"
  #         },
  #         "AITL": {
  #           "name": "Total Inventory",
  #           "value": "417.600000"
  #         },
  #         "APPY": {
  #           "name": "Prepaid Expenses",
  #           "value": "685.000000"
  #         },
  #         "SOCA": {
  #           "name": "Other Current Assets, Total",
  #           "value": "155.100000"
  #         },
  #         "ATCA": {
  #           "name": "Total Current Assets",
  #           "value": "7078.100000"
  #         },
  #         "APTC": {
  #           "name": "Property/Plant/Equipment, Total - Gross",
  #           "value": "32942.200000"
  #         },
  #         "ADEP": {
  #           "name": "Accumulated Depreciation, Total",
  #           "value": "-21891.800000"
  #         },
  #         "APPN": {
  #           "name": "Property/Plant/Equipment, Total - Net",
  #           "value": "11050.400000"
  #         },
  #         "AGWI": {
  #           "name": "Goodwill, Net",
  #           "value": "11538.300000"
  #         },
  #         "AINT": {
  #           "name": "Intangibles, Net",
  #           "value": "2478.400000"
  #         },
  #         "SINV": {
  #           "name": "Long Term Investments",
  #           "value": "15565.000000"
  #         },
  #         "ALTR": {
  #           "name": "Note Receivable - Long Term",
  #           "value": "0.000000"
  #         },
  #         "SOLA": {
  #           "name": "Other Long Term Assets, Total",
  #           "value": "1204.600000"
  #         },
  #         "ATOT": {
  #           "name": "Total Assets",
  #           "value": "48914.800000"
  #         },
  #         "LAPB": {
  #           "name": "Accounts Payable",
  #           "value": "4441.500000"
  #         },
  #         "LAEX": {
  #           "name": "Accrued Expenses",
  #           "value": "976.400000"
  #         },
  #         "LSTD": {
  #           "name": "Notes Payable/Short Term Debt",
  #           "value": "0.000000"
  #         },
  #         "LCLD": {
  #           "name": "Current Port. of  LT Debt/Capital Leases",
  #           "value": "1880.200000"
  #         },
  #         "SOCL": {
  #           "name": "Other Current liabilities, Total",
  #           "value": "1496.300000"
  #         },
  #         "LTCL": {
  #           "name": "Total Current Liabilities",
  #           "value": "8794.400000"
  #         },
  #         "LLTD": {
  #           "name": "Long Term Debt",
  #           "value": "8783.900000"
  #         },
  #         "LTTD": {
  #           "name": "Total Long Term Debt",
  #           "value": "8783.900000"
  #         },
  #         "STLD": {
  #           "name": "Total Debt",
  #           "value": "10664.100000"
  #         },
  #         "SBDT": {
  #           "name": "Deferred Income Tax",
  #           "value": "515.100000"
  #         },
  #         "LMIN": {
  #           "name": "Minority Interest",
  #           "value": "-28.100000"
  #         },
  #         "SLTL": {
  #           "name": "Other Liabilities, Total",
  #           "value": "1011.700000"
  #         },
  #         "LTLL": {
  #           "name": "Total Liabilities",
  #           "value": "19077.000000"
  #         },
  #         "SCMS": {
  #           "name": "Common Stock, Total",
  #           "value": "4127.300000"
  #         },
  #         "QRED": {
  #           "name": "Retained Earnings (Accumulated Deficit)",
  #           "value": "27509.400000"
  #         },
  #         "QTSC": {
  #           "name": "Treasury Stock - Common",
  #           "value": "-31.700000"
  #         },
  #         "SOTE": {
  #           "name": "Other Equity, Total",
  #           "value": "-1767.200000"
  #         },
  #         "QTLE": {
  #           "name": "Total Equity",
  #           "value": "29837.800000"
  #         },
  #         "QTEL": {
  #           "name": "Total Liabilities & Shareholders' Equity",
  #           "value": "48914.800000"
  #         },
  #         "QTCO": {
  #           "name": "Total Common Shares Outstanding",
  #           "value": "16328.292000"
  #         },
  #         "STBP": {
  #           "name": "Tangible Book Value per Share, Common Eq",
  #           "value": "0.968940"
  #         }
  #       },
  #       "cash": {
  #         "ONET": {
  #           "name": "Net Income/Starting Line",
  #           "value": "3745.900000"
  #         },
  #         "SDED": {
  #           "name": "Depreciation/Depletion",
  #           "value": "2222.200000"
  #         },
  #         "SNCI": {
  #           "name": "Non-Cash Items",
  #           "value": "-1343.400000"
  #         },
  #         "SCTP": {
  #           "name": "Cash Taxes Paid",
  #           "value": "679.500000"
  #         },
  #         "SCIP": {
  #           "name": "Cash Interest Paid",
  #           "value": "385.100000"
  #         },
  #         "SOCF": {
  #           "name": "Changes in Working Capital",
  #           "value": "742.900000"
  #         },
  #         "OTLO": {
  #           "name": "Cash from Operating Activities",
  #           "value": "5367.600000"
  #         },
  #         "SCEX": {
  #           "name": "Capital Expenditures",
  #           "value": "-2057.900000"
  #         },
  #         "SICF": {
  #           "name": "Other Investing Cash Flow Items, Total",
  #           "value": "-270.600000"
  #         },
  #         "ITLI": {
  #           "name": "Cash from Investing Activities",
  #           "value": "-2328.500000"
  #         },
  #         "SFCF": {
  #           "name": "Financing Cash Flow Items",
  #           "value": "-395.600000"
  #         },
  #         "FCDP": {
  #           "name": "Total Cash Dividends Paid",
  #           "value": "-2856.600000"
  #         },
  #         "FPSS": {
  #           "name": "Issuance (Retirement) of Stock, Net",
  #           "value": "-25.600000"
  #         },
  #         "FPRD": {
  #           "name": "Issuance (Retirement) of Debt, Net",
  #           "value": "222.300000"
  #         },
  #         "FTLF": {
  #           "name": "Cash from Financing Activities",
  #           "value": "-3055.500000"
  #         },
  #         "SFEE": {
  #           "name": "Foreign Exchange Effects",
  #           "value": "4.200000"
  #         },
  #         "SNCC": {
  #           "name": "Net Change in Cash",
  #           "value": "-12.200000"
  #         }
  #       }
  #     },
  #     {
  #       "type": "Annual",
  #       "endDate": "2018-03-31",
  #       "fiscalYear": "2018",
  #       "income": {
  #         "SREV": {
  #           "name": "Revenue",
  #           "value": "17268.000000"
  #         },
  #         "RTLR": {
  #           "name": "Total Revenue",
  #           "value": "17268.000000"
  #         },
  #         "SCOR": {
  #           "name": "Cost of Revenue, Total",
  #           "value": "7179.600000"
  #         },
  #         "SGRP": {
  #           "name": "Gross Profit",
  #           "value": "10088.400000"
  #         },
  #         "SSGA": {
  #           "name": "Selling/General/Admin. Expenses, Total",
  #           "value": "5296.700000"
  #         },
  #         "SDPR": {
  #           "name": "Depreciation/Amortization",
  #           "value": "2251.500000"
  #         },
  #         "SINN": {
  #           "name": "Interest Exp.(Inc.),Net-Operating, Total",
  #           "value": "9.100000"
  #         },
  #         "SUIE": {
  #           "name": "Unusual Expense (Income)",
  #           "value": "-1899.400000"
  #         },
  #         "SOOE": {
  #           "name": "Other Operating Expenses, Total",
  #           "value": "-265.100000"
  #         },
  #         "ETOE": {
  #           "name": "Total Operating Expense",
  #           "value": "12572.400000"
  #         },
  #         "SOPI": {
  #           "name": "Operating Income",
  #           "value": "4695.600000"
  #         },
  #         "SNIN": {
  #           "name": "Interest Inc.(Exp.),Net-Non-Op., Total",
  #           "value": "1473.000000"
  #         },
  #         "SONT": {
  #           "name": "Other, Net",
  #           "value": "-13.700000"
  #         },
  #         "EIBT": {
  #           "name": "Net Income Before Taxes",
  #           "value": "6154.900000"
  #         },
  #         "TTAX": {
  #           "name": "Provision for Income Taxes",
  #           "value": "703.000000"
  #         },
  #         "TIAT": {
  #           "name": "Net Income After Taxes",
  #           "value": "5451.900000"
  #         },
  #         "CMIN": {
  #           "name": "Minority Interest",
  #           "value": "21.100000"
  #         },
  #         "NIBX": {
  #           "name": "Net Income Before Extra. Items",
  #           "value": "5473.000000"
  #         },
  #         "NINC": {
  #           "name": "Net Income",
  #           "value": "5473.000000"
  #         },
  #         "CIAC": {
  #           "name": "Income Available to Com Excl ExtraOrd",
  #           "value": "5473.000000"
  #         },
  #         "XNIC": {
  #           "name": "Income Available to Com Incl ExtraOrd",
  #           "value": "5473.000000"
  #         },
  #         "SDNI": {
  #           "name": "Diluted Net Income",
  #           "value": "5473.000000"
  #         },
  #         "SDWS": {
  #           "name": "Diluted Weighted Average Shares",
  #           "value": "16344.329000"
  #         },
  #         "SDBF": {
  #           "name": "Diluted EPS Excluding ExtraOrd Items",
  #           "value": "0.334860"
  #         },
  #         "DDPS1": {
  #           "name": "DPS - Common Stock Primary Issue",
  #           "value": "0.175000"
  #         },
  #         "VDES": {
  #           "name": "Diluted Normalized EPS",
  #           "value": "0.231920"
  #         }
  #       },
  #       "balance": {
  #         "ACSH": {
  #           "name": "Cash",
  #           "value": "402.200000"
  #         },
  #         "ACAE": {
  #           "name": "Cash & Equivalents",
  #           "value": "105.700000"
  #         },
  #         "ASTI": {
  #           "name": "Short Term Investments",
  #           "value": "17.000000"
  #         },
  #         "SCSI": {
  #           "name": "Cash and Short Term Investments",
  #           "value": "524.900000"
  #         },
  #         "AACR": {
  #           "name": "Accounts Receivable - Trade, Net",
  #           "value": "4601.700000"
  #         },
  #         "ATRC": {
  #           "name": "Total Receivables, Net",
  #           "value": "5261.400000"
  #         },
  #         "AITL": {
  #           "name": "Total Inventory",
  #           "value": "397.400000"
  #         },
  #         "APPY": {
  #           "name": "Prepaid Expenses",
  #           "value": "552.300000"
  #         },
  #         "SOCA": {
  #           "name": "Other Current Assets, Total",
  #           "value": "22.600000"
  #         },
  #         "ATCA": {
  #           "name": "Total Current Assets",
  #           "value": "6758.600000"
  #         },
  #         "APTC": {
  #           "name": "Property/Plant/Equipment, Total - Gross",
  #           "value": "32567.700000"
  #         },
  #         "ADEP": {
  #           "name": "Accumulated Depreciation, Total",
  #           "value": "-21113.600000"
  #         },
  #         "APPN": {
  #           "name": "Property/Plant/Equipment, Total - Net",
  #           "value": "11454.100000"
  #         },
  #         "AGWI": {
  #           "name": "Goodwill, Net",
  #           "value": "11372.200000"
  #         },
  #         "AINT": {
  #           "name": "Intangibles, Net",
  #           "value": "2596.900000"
  #         },
  #         "SINV": {
  #           "name": "Long Term Investments",
  #           "value": "14984.600000"
  #         },
  #         "ALTR": {
  #           "name": "Note Receivable - Long Term",
  #           "value": "0.000000"
  #         },
  #         "SOLA": {
  #           "name": "Other Long Term Assets, Total",
  #           "value": "1329.100000"
  #         },
  #         "ATOT": {
  #           "name": "Total Assets",
  #           "value": "48495.500000"
  #         },
  #         "LAPB": {
  #           "name": "Accounts Payable",
  #           "value": "4025.000000"
  #         },
  #         "LAEX": {
  #           "name": "Accrued Expenses",
  #           "value": "1014.900000"
  #         },
  #         "LSTD": {
  #           "name": "Notes Payable/Short Term Debt",
  #           "value": "0.000000"
  #         },
  #         "LCLD": {
  #           "name": "Current Port. of  LT Debt/Capital Leases",
  #           "value": "1823.600000"
  #         },
  #         "SOCL": {
  #           "name": "Other Current liabilities, Total",
  #           "value": "1565.900000"
  #         },
  #         "LTCL": {
  #           "name": "Total Current Liabilities",
  #           "value": "8429.400000"
  #         },
  #         "LLTD": {
  #           "name": "Long Term Debt",
  #           "value": "8667.600000"
  #         },
  #         "LTTD": {
  #           "name": "Total Long Term Debt",
  #           "value": "8667.600000"
  #         },
  #         "STLD": {
  #           "name": "Total Debt",
  #           "value": "10491.200000"
  #         },
  #         "SBDT": {
  #           "name": "Deferred Income Tax",
  #           "value": "535.600000"
  #         },
  #         "LMIN": {
  #           "name": "Minority Interest",
  #           "value": "-3.200000"
  #         },
  #         "SLTL": {
  #           "name": "Other Liabilities, Total",
  #           "value": "1151.400000"
  #         },
  #         "LTLL": {
  #           "name": "Total Liabilities",
  #           "value": "18780.800000"
  #         },
  #         "SCMS": {
  #           "name": "Common Stock, Total",
  #           "value": "4127.300000"
  #         },
  #         "QRED": {
  #           "name": "Retained Earnings (Accumulated Deficit)",
  #           "value": "26958.100000"
  #         },
  #         "QTSC": {
  #           "name": "Treasury Stock - Common",
  #           "value": "-32.700000"
  #         },
  #         "SOTE": {
  #           "name": "Other Equity, Total",
  #           "value": "-1338.000000"
  #         },
  #         "QTLE": {
  #           "name": "Total Equity",
  #           "value": "29714.700000"
  #         },
  #         "QTEL": {
  #           "name": "Total Liabilities & Shareholders' Equity",
  #           "value": "48495.500000"
  #         },
  #         "QTCO": {
  #           "name": "Total Common Shares Outstanding",
  #           "value": "16328.520820"
  #         },
  #         "STBP": {
  #           "name": "Tangible Book Value per Share, Common Eq",
  #           "value": "0.964300"
  #         }
  #       },
  #       "cash": {
  #         "ONET": {
  #           "name": "Net Income/Starting Line",
  #           "value": "6154.900000"
  #         },
  #         "SDED": {
  #           "name": "Depreciation/Depletion",
  #           "value": "2250.000000"
  #         },
  #         "SNCI": {
  #           "name": "Non-Cash Items",
  #           "value": "-3349.300000"
  #         },
  #         "SCTP": {
  #           "name": "Cash Taxes Paid",
  #           "value": "607.800000"
  #         },
  #         "SCIP": {
  #           "name": "Cash Interest Paid",
  #           "value": "379.900000"
  #         },
  #         "SOCF": {
  #           "name": "Changes in Working Capital",
  #           "value": "899.600000"
  #         },
  #         "OTLO": {
  #           "name": "Cash from Operating Activities",
  #           "value": "5955.200000"
  #         },
  #         "SCEX": {
  #           "name": "Capital Expenditures",
  #           "value": "-3473.400000"
  #         },
  #         "SICF": {
  #           "name": "Other Investing Cash Flow Items, Total",
  #           "value": "1522.700000"
  #         },
  #         "ITLI": {
  #           "name": "Cash from Investing Activities",
  #           "value": "-1950.700000"
  #         },
  #         "SFCF": {
  #           "name": "Financing Cash Flow Items",
  #           "value": "-326.000000"
  #         },
  #         "FCDP": {
  #           "name": "Total Cash Dividends Paid",
  #           "value": "-3346.300000"
  #         },
  #         "FPSS": {
  #           "name": "Issuance (Retirement) of Stock, Net",
  #           "value": "-25.000000"
  #         },
  #         "FPRD": {
  #           "name": "Issuance (Retirement) of Debt, Net",
  #           "value": "-311.900000"
  #         },
  #         "FTLF": {
  #           "name": "Cash from Financing Activities",
  #           "value": "-4009.200000"
  #         },
  #         "SFEE": {
  #           "name": "Foreign Exchange Effects",
  #           "value": "-4.200000"
  #         },
  #         "SNCC": {
  #           "name": "Net Change in Cash",
  #           "value": "-8.900000"
  #         }
  #       }
  #     },
  #     {
  #       "type": "Annual",
  #       "endDate": "2017-03-31",
  #       "fiscalYear": "2017",
  #       "income": {
  #         "SREV": {
  #           "name": "Revenue",
  #           "value": "16711.400000"
  #         },
  #         "RTLR": {
  #           "name": "Total Revenue",
  #           "value": "16711.400000"
  #         },
  #         "SCOR": {
  #           "name": "Cost of Revenue, Total",
  #           "value": "6483.700000"
  #         },
  #         "SGRP": {
  #           "name": "Gross Profit",
  #           "value": "10227.700000"
  #         },
  #         "SSGA": {
  #           "name": "Selling/General/Admin. Expenses, Total",
  #           "value": "5445.300000"
  #         },
  #         "SDPR": {
  #           "name": "Depreciation/Amortization",
  #           "value": "2242.000000"
  #         },
  #         "SINN": {
  #           "name": "Interest Exp.(Inc.),Net-Operating, Total",
  #           "value": "-5.100000"
  #         },
  #         "SUIE": {
  #           "name": "Unusual Expense (Income)",
  #           "value": "9.100000"
  #         },
  #         "SOOE": {
  #           "name": "Other Operating Expenses, Total",
  #           "value": "-221.200000"
  #         },
  #         "ETOE": {
  #           "name": "Total Operating Expense",
  #           "value": "13953.800000"
  #         },
  #         "SOPI": {
  #           "name": "Operating Income",
  #           "value": "2757.600000"
  #         },
  #         "SNIN": {
  #           "name": "Interest Inc.(Exp.),Net-Non-Op., Total",
  #           "value": "1757.300000"
  #         },
  #         "SONT": {
  #           "name": "Other, Net",
  #           "value": "0.500000"
  #         },
  #         "EIBT": {
  #           "name": "Net Income Before Taxes",
  #           "value": "4515.400000"
  #         },
  #         "TTAX": {
  #           "name": "Provision for Income Taxes",
  #           "value": "684.400000"
  #         },
  #         "TIAT": {
  #           "name": "Net Income After Taxes",
  #           "value": "3831.000000"
  #         },
  #         "CMIN": {
  #           "name": "Minority Interest",
  #           "value": "21.700000"
  #         },
  #         "NIBX": {
  #           "name": "Net Income Before Extra. Items",
  #           "value": "3852.700000"
  #         },
  #         "NINC": {
  #           "name": "Net Income",
  #           "value": "3852.700000"
  #         },
  #         "CIAC": {
  #           "name": "Income Available to Com Excl ExtraOrd",
  #           "value": "3852.700000"
  #         },
  #         "XNIC": {
  #           "name": "Income Available to Com Incl ExtraOrd",
  #           "value": "3852.700000"
  #         },
  #         "SDNI": {
  #           "name": "Diluted Net Income",
  #           "value": "3852.700000"
  #         },
  #         "SDWS": {
  #           "name": "Diluted Weighted Average Shares",
  #           "value": "16109.251000"
  #         },
  #         "SDBF": {
  #           "name": "Diluted EPS Excluding ExtraOrd Items",
  #           "value": "0.239160"
  #         },
  #         "DDPS1": {
  #           "name": "DPS - Common Stock Primary Issue",
  #           "value": "0.175000"
  #         },
  #         "VDES": {
  #           "name": "Diluted Normalized EPS",
  #           "value": "0.239640"
  #         }
  #       },
  #       "balance": {
  #         "ACSH": {
  #           "name": "Cash",
  #           "value": "369.700000"
  #         },
  #         "ACAE": {
  #           "name": "Cash & Equivalents",
  #           "value": "147.800000"
  #         },
  #         "ASTI": {
  #           "name": "Short Term Investments",
  #           "value": "16.300000"
  #         },
  #         "SCSI": {
  #           "name": "Cash and Short Term Investments",
  #           "value": "533.800000"
  #         },
  #         "AACR": {
  #           "name": "Accounts Receivable - Trade, Net",
  #           "value": "4453.100000"
  #         },
  #         "ATRC": {
  #           "name": "Total Receivables, Net",
  #           "value": "5222.100000"
  #         },
  #         "AITL": {
  #           "name": "Total Inventory",
  #           "value": "352.200000"
  #         },
  #         "APPY": {
  #           "name": "Prepaid Expenses",
  #           "value": "540.300000"
  #         },
  #         "SOCA": {
  #           "name": "Other Current Assets, Total",
  #           "value": "106.100000"
  #         },
  #         "ATCA": {
  #           "name": "Total Current Assets",
  #           "value": "6754.500000"
  #         },
  #         "APTC": {
  #           "name": "Property/Plant/Equipment, Total - Gross",
  #           "value": "32864.500000"
  #         },
  #         "ADEP": {
  #           "name": "Accumulated Depreciation, Total",
  #           "value": "-20971.330000"
  #         },
  #         "APPN": {
  #           "name": "Property/Plant/Equipment, Total - Net",
  #           "value": "11456.100000"
  #         },
  #         "AGWI": {
  #           "name": "Goodwill, Net",
  #           "value": "11164.600000"
  #         },
  #         "AINT": {
  #           "name": "Intangibles, Net",
  #           "value": "1908.200000"
  #         },
  #         "SINV": {
  #           "name": "Long Term Investments",
  #           "value": "14424.900000"
  #         },
  #         "ALTR": {
  #           "name": "Note Receivable - Long Term",
  #           "value": "1100.500000"
  #         },
  #         "SOLA": {
  #           "name": "Other Long Term Assets, Total",
  #           "value": "1661.300000"
  #         },
  #         "ATOT": {
  #           "name": "Total Assets",
  #           "value": "48470.100000"
  #         },
  #         "LAPB": {
  #           "name": "Accounts Payable",
  #           "value": "3618.600000"
  #         },
  #         "LAEX": {
  #           "name": "Accrued Expenses",
  #           "value": "1128.600000"
  #         },
  #         "LSTD": {
  #           "name": "Notes Payable/Short Term Debt",
  #           "value": "0.000000"
  #         },
  #         "LCLD": {
  #           "name": "Current Port. of  LT Debt/Capital Leases",
  #           "value": "3133.300000"
  #         },
  #         "SOCL": {
  #           "name": "Other Current liabilities, Total",
  #           "value": "1549.600000"
  #         },
  #         "LTCL": {
  #           "name": "Total Current Liabilities",
  #           "value": "9430.100000"
  #         },
  #         "LLTD": {
  #           "name": "Long Term Debt",
  #           "value": "8097.800000"
  #         },
  #         "LTTD": {
  #           "name": "Total Long Term Debt",
  #           "value": "8097.800000"
  #         },
  #         "STLD": {
  #           "name": "Total Debt",
  #           "value": "11231.100000"
  #         },
  #         "SBDT": {
  #           "name": "Deferred Income Tax",
  #           "value": "572.800000"
  #         },
  #         "LMIN": {
  #           "name": "Minority Interest",
  #           "value": "22.400000"
  #         },
  #         "SLTL": {
  #           "name": "Other Liabilities, Total",
  #           "value": "2128.200000"
  #         },
  #         "LTLL": {
  #           "name": "Total Liabilities",
  #           "value": "20251.300000"
  #         },
  #         "SCMS": {
  #           "name": "Common Stock, Total",
  #           "value": "4127.300000"
  #         },
  #         "QRED": {
  #           "name": "Retained Earnings (Accumulated Deficit)",
  #           "value": "28620.400000"
  #         },
  #         "QTSC": {
  #           "name": "Treasury Stock - Common",
  #           "value": "-32.500000"
  #         },
  #         "SOTE": {
  #           "name": "Other Equity, Total",
  #           "value": "-4496.400000"
  #         },
  #         "QTLE": {
  #           "name": "Total Equity",
  #           "value": "28218.800000"
  #         },
  #         "QTEL": {
  #           "name": "Total Liabilities & Shareholders' Equity",
  #           "value": "48470.100000"
  #         },
  #         "QTCO": {
  #           "name": "Total Common Shares Outstanding",
  #           "value": "16328.454450"
  #         },
  #         "STBP": {
  #           "name": "Tangible Book Value per Share, Common Eq",
  #           "value": "0.927580"
  #         }
  #       },
  #       "cash": {
  #         "ONET": {
  #           "name": "Net Income/Starting Line",
  #           "value": "4515.400000"
  #         },
  #         "SDED": {
  #           "name": "Depreciation/Depletion",
  #           "value": "2238.900000"
  #         },
  #         "SNCI": {
  #           "name": "Non-Cash Items",
  #           "value": "-1769.100000"
  #         },
  #         "SCTP": {
  #           "name": "Cash Taxes Paid",
  #           "value": "833.800000"
  #         },
  #         "SCIP": {
  #           "name": "Cash Interest Paid",
  #           "value": "351.300000"
  #         },
  #         "SOCF": {
  #           "name": "Changes in Working Capital",
  #           "value": "329.500000"
  #         },
  #         "OTLO": {
  #           "name": "Cash from Operating Activities",
  #           "value": "5314.700000"
  #         },
  #         "SCEX": {
  #           "name": "Capital Expenditures",
  #           "value": "-2518.300000"
  #         },
  #         "SICF": {
  #           "name": "Other Investing Cash Flow Items, Total",
  #           "value": "-2313.900000"
  #         },
  #         "ITLI": {
  #           "name": "Cash from Investing Activities",
  #           "value": "-4832.200000"
  #         },
  #         "SFCF": {
  #           "name": "Financing Cash Flow Items",
  #           "value": "-339.700000"
  #         },
  #         "FCDP": {
  #           "name": "Total Cash Dividends Paid",
  #           "value": "-2815.500000"
  #         },
  #         "FPSS": {
  #           "name": "Issuance (Retirement) of Stock, Net",
  #           "value": "1575.200000"
  #         },
  #         "FPRD": {
  #           "name": "Issuance (Retirement) of Debt, Net",
  #           "value": "1157.600000"
  #         },
  #         "FTLF": {
  #           "name": "Cash from Financing Activities",
  #           "value": "-422.400000"
  #         },
  #         "SFEE": {
  #           "name": "Foreign Exchange Effects",
  #           "value": "11.900000"
  #         },
  #         "SNCC": {
  #           "name": "Net Change in Cash",
  #           "value": "72.000000"
  #         }
  #       }
  #     },
  #     {
  #       "type": "Annual",
  #       "endDate": "2016-03-31",
  #       "fiscalYear": "2016",
  #       "income": {
  #         "SREV": {
  #           "name": "Revenue",
  #           "value": "16961.200000"
  #         },
  #         "RTLR": {
  #           "name": "Total Revenue",
  #           "value": "16961.200000"
  #         },
  #         "SCOR": {
  #           "name": "Cost of Revenue, Total",
  #           "value": "6251.200000"
  #         },
  #         "SGRP": {
  #           "name": "Gross Profit",
  #           "value": "10710.000000"
  #         },
  #         "SSGA": {
  #           "name": "Selling/General/Admin. Expenses, Total",
  #           "value": "5842.400000"
  #         },
  #         "SDPR": {
  #           "name": "Depreciation/Amortization",
  #           "value": "2151.900000"
  #         },
  #         "SINN": {
  #           "name": "Interest Exp.(Inc.),Net-Operating, Total",
  #           "value": "-101.900000"
  #         },
  #         "SUIE": {
  #           "name": "Unusual Expense (Income)",
  #           "value": "147.000000"
  #         },
  #         "SOOE": {
  #           "name": "Other Operating Expenses, Total",
  #           "value": "-148.500000"
  #         },
  #         "ETOE": {
  #           "name": "Total Operating Expense",
  #           "value": "14142.100000"
  #         },
  #         "SOPI": {
  #           "name": "Operating Income",
  #           "value": "2819.100000"
  #         },
  #         "SNIN": {
  #           "name": "Interest Inc.(Exp.),Net-Non-Op., Total",
  #           "value": "1763.500000"
  #         },
  #         "SONT": {
  #           "name": "Other, Net",
  #           "value": "-1.800000"
  #         },
  #         "EIBT": {
  #           "name": "Net Income Before Taxes",
  #           "value": "4580.800000"
  #         },
  #         "TTAX": {
  #           "name": "Provision for Income Taxes",
  #           "value": "722.500000"
  #         },
  #         "TIAT": {
  #           "name": "Net Income After Taxes",
  #           "value": "3858.300000"
  #         },
  #         "CMIN": {
  #           "name": "Minority Interest",
  #           "value": "12.500000"
  #         },
  #         "NIBX": {
  #           "name": "Net Income Before Extra. Items",
  #           "value": "3870.800000"
  #         },
  #         "NINC": {
  #           "name": "Net Income",
  #           "value": "3870.800000"
  #         },
  #         "CIAC": {
  #           "name": "Income Available to Com Excl ExtraOrd",
  #           "value": "3870.800000"
  #         },
  #         "XNIC": {
  #           "name": "Income Available to Com Incl ExtraOrd",
  #           "value": "3870.800000"
  #         },
  #         "SDNI": {
  #           "name": "Diluted Net Income",
  #           "value": "3870.800000"
  #         },
  #         "SDWS": {
  #           "name": "Diluted Weighted Average Shares",
  #           "value": "15952.029000"
  #         },
  #         "SDBF": {
  #           "name": "Diluted EPS Excluding ExtraOrd Items",
  #           "value": "0.242650"
  #         },
  #         "DDPS1": {
  #           "name": "DPS - Common Stock Primary Issue",
  #           "value": "0.175000"
  #         },
  #         "VDES": {
  #           "name": "Diluted Normalized EPS",
  #           "value": "0.250410"
  #         }
  #       },
  #       "balance": {
  #         "ACAE": {
  #           "name": "Cash & Equivalents",
  #           "value": "461.800000"
  #         },
  #         "SCSI": {
  #           "name": "Cash and Short Term Investments",
  #           "value": "461.800000"
  #         },
  #         "AACR": {
  #           "name": "Accounts Receivable - Trade, Net",
  #           "value": "3178.400000"
  #         },
  #         "ATRC": {
  #           "name": "Total Receivables, Net",
  #           "value": "3889.200000"
  #         },
  #         "AITL": {
  #           "name": "Total Inventory",
  #           "value": "319.700000"
  #         },
  #         "APPY": {
  #           "name": "Prepaid Expenses",
  #           "value": "477.200000"
  #         },
  #         "SOCA": {
  #           "name": "Other Current Assets, Total",
  #           "value": "17.500000"
  #         },
  #         "ATCA": {
  #           "name": "Total Current Assets",
  #           "value": "5165.400000"
  #         },
  #         "APTC": {
  #           "name": "Property/Plant/Equipment, Total - Gross",
  #           "value": "30033.600000"
  #         },
  #         "ADEP": {
  #           "name": "Accumulated Depreciation, Total",
  #           "value": "-18879.600000"
  #         },
  #         "APPN": {
  #           "name": "Property/Plant/Equipment, Total - Net",
  #           "value": "11154.000000"
  #         },
  #         "AGWI": {
  #           "name": "Goodwill, Net",
  #           "value": "11090.300000"
  #         },
  #         "AINT": {
  #           "name": "Intangibles, Net",
  #           "value": "1878.100000"
  #         },
  #         "SINV": {
  #           "name": "Long Term Investments",
  #           "value": "11233.700000"
  #         },
  #         "ALTR": {
  #           "name": "Note Receivable - Long Term",
  #           "value": "1536.300000"
  #         },
  #         "SOLA": {
  #           "name": "Other Long Term Assets, Total",
  #           "value": "1507.900000"
  #         },
  #         "ATOT": {
  #           "name": "Total Assets",
  #           "value": "43565.700000"
  #         },
  #         "LAPB": {
  #           "name": "Accounts Payable",
  #           "value": "3437.700000"
  #         },
  #         "LAEX": {
  #           "name": "Accrued Expenses",
  #           "value": "1046.600000"
  #         },
  #         "LSTD": {
  #           "name": "Notes Payable/Short Term Debt",
  #           "value": "0.000000"
  #         },
  #         "LCLD": {
  #           "name": "Current Port. of  LT Debt/Capital Leases",
  #           "value": "685.700000"
  #         },
  #         "SOCL": {
  #           "name": "Other Current liabilities, Total",
  #           "value": "1369.900000"
  #         },
  #         "LTCL": {
  #           "name": "Total Current Liabilities",
  #           "value": "6539.900000"
  #         },
  #         "LLTD": {
  #           "name": "Long Term Debt",
  #           "value": "9065.100000"
  #         },
  #         "LCLO": {
  #           "name": "Capital Lease Obligations",
  #           "value": "189.900000"
  #         },
  #         "LTTD": {
  #           "name": "Total Long Term Debt",
  #           "value": "9255.000000"
  #         },
  #         "STLD": {
  #           "name": "Total Debt",
  #           "value": "9940.700000"
  #         },
  #         "SBDT": {
  #           "name": "Deferred Income Tax",
  #           "value": "585.300000"
  #         },
  #         "LMIN": {
  #           "name": "Minority Interest",
  #           "value": "35.700000"
  #         },
  #         "SLTL": {
  #           "name": "Other Liabilities, Total",
  #           "value": "2183.000000"
  #         },
  #         "LTLL": {
  #           "name": "Total Liabilities",
  #           "value": "18598.900000"
  #         },
  #         "SCMS": {
  #           "name": "Common Stock, Total",
  #           "value": "2634.000000"
  #         },
  #         "QRED": {
  #           "name": "Retained Earnings (Accumulated Deficit)",
  #           "value": "27308.700000"
  #         },
  #         "QTSC": {
  #           "name": "Treasury Stock - Common",
  #           "value": "-30.600000"
  #         },
  #         "SOTE": {
  #           "name": "Other Equity, Total",
  #           "value": "-4945.300000"
  #         },
  #         "QTLE": {
  #           "name": "Total Equity",
  #           "value": "24966.800000"
  #         },
  #         "QTEL": {
  #           "name": "Total Liabilities & Shareholders' Equity",
  #           "value": "43565.700000"
  #         },
  #         "QTCO": {
  #           "name": "Total Common Shares Outstanding",
  #           "value": "15942.906510"
  #         },
  #         "STBP": {
  #           "name": "Tangible Book Value per Share, Common Eq",
  #           "value": "0.752590"
  #         }
  #       },
  #       "cash": {
  #         "ONET": {
  #           "name": "Net Income/Starting Line",
  #           "value": "4580.800000"
  #         },
  #         "SDED": {
  #           "name": "Depreciation/Depletion",
  #           "value": "2148.800000"
  #         },
  #         "SNCI": {
  #           "name": "Non-Cash Items",
  #           "value": "-1729.700000"
  #         },
  #         "SCTP": {
  #           "name": "Cash Taxes Paid",
  #           "value": "658.200000"
  #         },
  #         "SCIP": {
  #           "name": "Cash Interest Paid",
  #           "value": "335.600000"
  #         },
  #         "SOCF": {
  #           "name": "Changes in Working Capital",
  #           "value": "-352.200000"
  #         },
  #         "OTLO": {
  #           "name": "Cash from Operating Activities",
  #           "value": "4647.700000"
  #         },
  #         "SCEX": {
  #           "name": "Capital Expenditures",
  #           "value": "-2103.300000"
  #         },
  #         "SICF": {
  #           "name": "Other Investing Cash Flow Items, Total",
  #           "value": "-636.700000"
  #         },
  #         "ITLI": {
  #           "name": "Cash from Investing Activities",
  #           "value": "-2740.000000"
  #         },
  #         "SFCF": {
  #           "name": "Financing Cash Flow Items",
  #           "value": "-338.900000"
  #         },
  #         "FCDP": {
  #           "name": "Total Cash Dividends Paid",
  #           "value": "-2789.200000"
  #         },
  #         "FPSS": {
  #           "name": "Issuance (Retirement) of Stock, Net",
  #           "value": "-44.100000"
  #         },
  #         "FPRD": {
  #           "name": "Issuance (Retirement) of Debt, Net",
  #           "value": "1128.700000"
  #         },
  #         "FTLF": {
  #           "name": "Cash from Financing Activities",
  #           "value": "-2043.500000"
  #         },
  #         "SFEE": {
  #           "name": "Foreign Exchange Effects",
  #           "value": "34.800000"
  #         },
  #         "SNCC": {
  #           "name": "Net Change in Cash",
  #           "value": "-101.000000"
  #         }
  #       }
  #     },
  #     {
  #       "type": "Annual",
  #       "endDate": "2015-03-31",
  #       "fiscalYear": "2015",
  #       "income": {
  #         "SREV": {
  #           "name": "Revenue",
  #           "value": "17222.900000"
  #         },
  #         "RTLR": {
  #           "name": "Total Revenue",
  #           "value": "17222.900000"
  #         },
  #         "SCOR": {
  #           "name": "Cost of Revenue, Total",
  #           "value": "5826.500000"
  #         },
  #         "SGRP": {
  #           "name": "Gross Profit",
  #           "value": "11396.400000"
  #         },
  #         "SSGA": {
  #           "name": "Selling/General/Admin. Expenses, Total",
  #           "value": "6966.900000"
  #         },
  #         "SDPR": {
  #           "name": "Depreciation/Amortization",
  #           "value": "2164.500000"
  #         },
  #         "SINN": {
  #           "name": "Interest Exp.(Inc.),Net-Operating, Total",
  #           "value": "-28.600000"
  #         },
  #         "SUIE": {
  #           "name": "Unusual Expense (Income)",
  #           "value": "11.700000"
  #         },
  #         "SOOE": {
  #           "name": "Other Operating Expenses, Total",
  #           "value": "-662.200000"
  #         },
  #         "ETOE": {
  #           "name": "Total Operating Expense",
  #           "value": "14278.800000"
  #         },
  #         "SOPI": {
  #           "name": "Operating Income",
  #           "value": "2944.100000"
  #         },
  #         "SNIN": {
  #           "name": "Interest Inc.(Exp.),Net-Non-Op., Total",
  #           "value": "1515.400000"
  #         },
  #         "SONT": {
  #           "name": "Other, Net",
  #           "value": "3.500000"
  #         },
  #         "EIBT": {
  #           "name": "Net Income Before Taxes",
  #           "value": "4463.000000"
  #         },
  #         "TTAX": {
  #           "name": "Provision for Income Taxes",
  #           "value": "678.500000"
  #         },
  #         "TIAT": {
  #           "name": "Net Income After Taxes",
  #           "value": "3784.500000"
  #         },
  #         "CMIN": {
  #           "name": "Minority Interest",
  #           "value": "-3.000000"
  #         },
  #         "NIBX": {
  #           "name": "Net Income Before Extra. Items",
  #           "value": "3781.500000"
  #         },
  #         "NINC": {
  #           "name": "Net Income",
  #           "value": "3781.500000"
  #         },
  #         "CIAC": {
  #           "name": "Income Available to Com Excl ExtraOrd",
  #           "value": "3781.500000"
  #         },
  #         "XNIC": {
  #           "name": "Income Available to Com Incl ExtraOrd",
  #           "value": "3781.500000"
  #         },
  #         "SDNI": {
  #           "name": "Diluted Net Income",
  #           "value": "3781.500000"
  #         },
  #         "SDWS": {
  #           "name": "Diluted Weighted Average Shares",
  #           "value": "15977.008000"
  #         },
  #         "SDBF": {
  #           "name": "Diluted EPS Excluding ExtraOrd Items",
  #           "value": "0.236680"
  #         },
  #         "DDPS1": {
  #           "name": "DPS - Common Stock Primary Issue",
  #           "value": "0.175000"
  #         },
  #         "VDES": {
  #           "name": "Diluted Normalized EPS",
  #           "value": "0.237300"
  #         }
  #       },
  #       "balance": {
  #         "ACAE": {
  #           "name": "Cash & Equivalents",
  #           "value": "562.800000"
  #         },
  #         "SCSI": {
  #           "name": "Cash and Short Term Investments",
  #           "value": "562.800000"
  #         },
  #         "AACR": {
  #           "name": "Accounts Receivable - Trade, Net",
  #           "value": "3885.200000"
  #         },
  #         "ATRC": {
  #           "name": "Total Receivables, Net",
  #           "value": "3885.200000"
  #         },
  #         "AITL": {
  #           "name": "Total Inventory",
  #           "value": "289.800000"
  #         },
  #         "SOCA": {
  #           "name": "Other Current Assets, Total",
  #           "value": "29.800000"
  #         },
  #         "ATCA": {
  #           "name": "Total Current Assets",
  #           "value": "4767.600000"
  #         },
  #         "APPN": {
  #           "name": "Property/Plant/Equipment, Total - Net",
  #           "value": "10683.200000"
  #         },
  #         "AINT": {
  #           "name": "Intangibles, Net",
  #           "value": "11948.600000"
  #         },
  #         "SINV": {
  #           "name": "Long Term Investments",
  #           "value": "11114.500000"
  #         },
  #         "ALTR": {
  #           "name": "Note Receivable - Long Term",
  #           "value": "2007.000000"
  #         },
  #         "SOLA": {
  #           "name": "Other Long Term Assets, Total",
  #           "value": "1545.900000"
  #         },
  #         "ATOT": {
  #           "name": "Total Assets",
  #           "value": "42066.800000"
  #         },
  #         "LPBA": {
  #           "name": "Payable/Accrued",
  #           "value": "4464.300000"
  #         },
  #         "LSTD": {
  #           "name": "Notes Payable/Short Term Debt",
  #           "value": "0.000000"
  #         },
  #         "LCLD": {
  #           "name": "Current Port. of  LT Debt/Capital Leases",
  #           "value": "174.400000"
  #         },
  #         "SOCL": {
  #           "name": "Other Current liabilities, Total",
  #           "value": "1118.100000"
  #         },
  #         "LTCL": {
  #           "name": "Total Current Liabilities",
  #           "value": "5756.800000"
  #         },
  #         "LLTD": {
  #           "name": "Long Term Debt",
  #           "value": "8804.400000"
  #         },
  #         "LTTD": {
  #           "name": "Total Long Term Debt",
  #           "value": "8804.400000"
  #         },
  #         "STLD": {
  #           "name": "Total Debt",
  #           "value": "8978.800000"
  #         },
  #         "SBDT": {
  #           "name": "Deferred Income Tax",
  #           "value": "521.700000"
  #         },
  #         "LMIN": {
  #           "name": "Minority Interest",
  #           "value": "34.600000"
  #         },
  #         "SLTL": {
  #           "name": "Other Liabilities, Total",
  #           "value": "2216.000000"
  #         },
  #         "LTLL": {
  #           "name": "Total Liabilities",
  #           "value": "17333.500000"
  #         },
  #         "SCMS": {
  #           "name": "Common Stock, Total",
  #           "value": "2634.000000"
  #         },
  #         "QRED": {
  #           "name": "Retained Earnings (Accumulated Deficit)",
  #           "value": "22099.300000"
  #         },
  #         "QTLE": {
  #           "name": "Total Equity",
  #           "value": "24733.300000"
  #         },
  #         "QTEL": {
  #           "name": "Total Liabilities & Shareholders' Equity",
  #           "value": "42066.800000"
  #         },
  #         "QTCO": {
  #           "name": "Total Common Shares Outstanding",
  #           "value": "15941.687640"
  #         },
  #         "STBP": {
  #           "name": "Tangible Book Value per Share, Common Eq",
  #           "value": "0.801970"
  #         }
  #       },
  #       "cash": {
  #         "ONET": {
  #           "name": "Net Income/Starting Line",
  #           "value": "4463.000000"
  #         },
  #         "SDED": {
  #           "name": "Depreciation/Depletion",
  #           "value": "2161.400000"
  #         },
  #         "SNCI": {
  #           "name": "Non-Cash Items",
  #           "value": "-1539.900000"
  #         },
  #         "SCTP": {
  #           "name": "Cash Taxes Paid",
  #           "value": "598.200000"
  #         },
  #         "SCIP": {
  #           "name": "Cash Interest Paid",
  #           "value": "307.300000"
  #         },
  #         "SOCF": {
  #           "name": "Changes in Working Capital",
  #           "value": "702.100000"
  #         },
  #         "OTLO": {
  #           "name": "Cash from Operating Activities",
  #           "value": "5786.600000"
  #         },
  #         "SCEX": {
  #           "name": "Capital Expenditures",
  #           "value": "-3203.600000"
  #         },
  #         "SICF": {
  #           "name": "Other Investing Cash Flow Items, Total",
  #           "value": "-353.300000"
  #         },
  #         "ITLI": {
  #           "name": "Cash from Investing Activities",
  #           "value": "-3556.900000"
  #         },
  #         "SFCF": {
  #           "name": "Financing Cash Flow Items",
  #           "value": "-315.600000"
  #         },
  #         "FCDP": {
  #           "name": "Total Cash Dividends Paid",
  #           "value": "-2677.500000"
  #         },
  #         "FPSS": {
  #           "name": "Issuance (Retirement) of Stock, Net",
  #           "value": "-54.700000"
  #         },
  #         "FPRD": {
  #           "name": "Issuance (Retirement) of Debt, Net",
  #           "value": "737.200000"
  #         },
  #         "FTLF": {
  #           "name": "Cash from Financing Activities",
  #           "value": "-2310.600000"
  #         },
  #         "SFEE": {
  #           "name": "Foreign Exchange Effects",
  #           "value": "21.200000"
  #         },
  #         "SNCC": {
  #           "name": "Net Change in Cash",
  #           "value": "-59.700000"
  #         }
  #       }
  #     }
  #   ]
  # }

  sgx.get_snapshot_report(ric)
  # {
  #   "Business Summary": "Singapore Telecommunications Ltd is a communications technology company. The Company provides a range of telecommunications and digital services to consumers and businesses across Asia, Australia, Africa and the United States. Its segments include Group Consumer, Group Enterprise and Group Digital Life. Consumer segment comprises the consumer businesses across Singapore and Australia, as well as the Company's investments, such as Advanced Info Service Public Company Limited (AIS) in Thailand, Bharti Airtel Limited (Airtel) in India, Africa and South Asia, Globe Telecom, Inc. (Globe) in the Philippines and PT Telekomunikasi Selular (Telkomsel) in Indonesia. Group Enterprise segment comprises the business groups across Singapore, Australia, the United States, Europe and the region, and focuses on the enterprise markets. Group Digital Life segment focuses on using the Internet technologies and assets of the Company's operating companies by entering adjacent businesses.",
  #   "Financial Summary": "BRIEF: For the fiscal year ended 31 March 2020, Singapore Telecommunications Limited revenues decreased 5% to SP$16.54B. Net income decreased 65% to SP$1.07B. Revenues reflect a decrease in demand for the Company's products and services due to unfavorable market conditions. Net income also reflects Share of exceptional item (post-tax) decrease from SP$301.1M (income) to SP$1.81B (expense), Gain on sale and leaseback decrease from SP$42.4M (income) to SP$0K.",
  #   "contactInfo": {
  #     "lastUpdated": "2020-06-24T20:37:51",
  #     "streetAddress": [
  #       "31 Exeter Road, Comcentre",
  #       null,
  #       null
  #     ],
  #     "city": null,
  #     "state": null,
  #     "postalCode": "239732",
  #     "country": {
  #       "code": "SGP",
  #       "name": "Singapore"
  #     }
  #   },
  #   "industries": [
  #     "Integrated Telecommunications Services",
  #     "Integrated Telecommunications Services - NEC",
  #     "Integrated Telecommunications Services",
  #     "Communications Services",
  #     "Services",
  #     "Cellular and Other Wireless Telecommunications",
  #     "Wired Telecommunications Carriers",
  #     "On-Line Information Services",
  #     "Cable and Other Program Distribution",
  #     "Other Electronic Parts and Equipment Wholesalers",
  #     "Engineering Services",
  #     "Offices of  Other Holding Companies",
  #     "Radiotelephone Communications",
  #     "Telephon Communications Not Radio",
  #     "Information Retrieval Services",
  #     "Cable And Other Pay Tv Services",
  #     "Electronic Parts And Equipment",
  #     "Engineering Services",
  #     "Holding Companies, Nec"
  #   ],
  #   "forecast": {
  #     "consensusType": "Mean",
  #     "fiscalYear": "2021",
  #     "ConsRecom": "2",
  #     "TargetPrice": "3.21072",
  #     "ProjLTGrowthRate": "-99999.99000",
  #     "ProjPE": "15.87808",
  #     "ProjSales": "15789.15240",
  #     "ProjSalesQ": "3623.41000",
  #     "ProjEPS": "0.15682",
  #     "ProjEPSQ": "0.04000",
  #     "ProjProfit": "3337.40870",
  #     "ProjDPS": "0.12256"
  #   },
  #   "website": "https://www.singtel.com/",
  #   "email": "investor@singtel.com"
  # }
```

Acronyms: [https://interactivebrokers.github.io/tws-api/fundamental_ratios_tags.html](https://interactivebrokers.github.io/tws-api/fundamental_ratios_tags.html)
