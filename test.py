import json
from sgx import SGX

api = SGX()

stock_code = 'Z74'
ibm_code = '1T75'
ric = 'STEL.SI'


def pretty_print(data):
  print(
      json.dumps(
          data,
          indent=2
      )
  )

pretty_print(api.get_all_stocks())
# pretty_print(sgx.get_corporate_info(ibm_code))
# pretty_print(api.get_ric_by_stock_code('Z74'))

# print(sgx.get_all_stocks_names())
# print(sgx.get_basic_info_by_stock('Z74'))
# print(sgx.get_historical_data_by_stock('Z74', period=HistoricPeriods.ONE_YEAR))
# print(sgx.get_all_stocks_attributes())
# print(sgx.get_stock_announcements_by_stock('Z74'))
# print(json.dumps(sgx.get_general_fundamentals_by_stock('STEL.SI'), indent=2))
