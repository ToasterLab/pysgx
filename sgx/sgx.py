# pylint: disable=no-member
#!/usr/bin/env python
import datetime
import json

import requests
import xmltodict

from sgx.constants import HistoricPeriods
from sgx.soap import Soap


class SGX:
  def __init__(self, api_version=None, token=None):
    self.api_version = api_version
    self.token = token
    if self.api_version is None:
      self.api_version = self.get_api_version()
    if self.token is None:
      self.token = self.get_token()
    pass

  def do_json_request(self, url, params={}):
    resp = requests.get(url, params=params)
    if resp.status_code != requests.codes.ok:
      raise Exception(f"Http {resp.status_code}: {resp.text}")
    data = resp.json()
    return data

  def do_sgx_json_request(self, url, params={}):
    data = self.do_json_request(url, params)
    if 'meta' not in data or 'code' not in data['meta'] or data['meta']['code'] != "200":
      raise Exception(f"Returned code {data['meta']['code']}")
    return data['data']

  def do_soap_request(self, url, payload, params={}):
    resp = requests.get(url, params=params, data=payload, headers={
        'Content-Type': 'application/soap+xml'
    })
    if resp.status_code != requests.codes.ok:
      raise Exception(f"Http {resp.status_code}: {resp.text} {resp.content}")
    data = xmltodict.parse(
        resp.content
    )
    return data

  def get_api_version(self):
    data = self.do_json_request('https://www.sgx.com/config/appconfig.json')
    api_version = data['cms']['apiVersion']
    self.api_version = api_version
    return api_version

  def get_token(self):
    data = self.do_json_request(
        'https://api2.sgx.com/content-api',
        params=dict(
            queryId=self.api_version+':stockfacts_screener_token'
        )
    )
    token = data['data']['stockfactsScreenerToken']
    self.token = token
    return token

  def get_stocks(self, start=0, size=250):
    params = dict(start=start, size=size)
    data = self.do_json_request(
        'https://api.sgx.com/securities/v1.1/gtis',
        params=params
    )
    return data

  def get_all_stocks(self):
    """
    Fetches a list of all mainboard stocks

    Returns:
        [
            {
                'issue': 'GTI2013',
                'year': 2013,
                'companyName':
                'SYNEAR FOOD HOLDINGS LIMITED',
                'rank': 396,
                'adjustment': -2,
                'baseScore': 34.0,
                'totalScore': 32.0,
                'stockCode': 'Z75',
                'isinCode': 'BMG8648Q1069'
            },
            ...
        ]
    """
    initial_data = self.get_stocks()
    total_pages = initial_data['meta']['totalPages']
    total_data = initial_data['data']
    for i in range(1, total_pages):
      additional_data = self.get_stocks(start=i)['data']
      total_data.extend(additional_data)
    return total_data

  def get_all_stocks_attributes(self):
    payload = Soap.request_screener_all()
    resp = self.do_soap_request(
        "https://apitrkd.trkd-hs.com/apitrkd/api/Screener/Screener.svc",
        payload,
        params=dict(
            id='public',
            token=self.token
        ),
    )
    return Soap.parse_screener_all(resp)

  def get_basic_info(self, stock_code):
    """
    Fetches basic information about a stock, given its stock code

    Arguments:
        stock_code {[str]} -- the short code for the stock, e.g. Z74 for SingTel

    Returns:
        {
            'fullName': 'SINGTEL',
            'stockCode': 'Z74',
            'ibmCode': '1T75',
            'isinCode': 'SG1T75931496',
            'chineseName': '新电信',
            'fisn': None
        }
    """
    params = {"stock-code": stock_code}
    data = self.do_sgx_json_request(
        'https://api.sgx.com/marketmetadata/v2',
        params=params
    )
    if len(data) != 1:
      raise Exception("Invalid basic info")
    basic_info = data[0]
    return basic_info

  def get_historical_data(self, stock_code, period=HistoricPeriods.ONE_YEAR):
    """
    fetches historical prices of a stock over a given period

    Arguments:
        stock_code {[str]} -- the short code for the stock, e.g. Z74 for SingTel

    Keyword Arguments:
        period {[str]} -- HistoricPeriods defined in constants.py (default: {HistoricPeriods.ONE_YEAR})

    Returns:
        [{
            'change_vs_pc_percentage': None,
            'cur': 'SGD',
            'ptd': '20190718',
            'pv': 3.52,
            'h': 3.54,
            'lt': 3.54,
            'trading_time': '20190719_091600',
            'l': 3.52,
            'type': 'stocks',
            'dp': None,
            'n': 'SingTel',
            'o': 3.54,
            'change_vs_pc': None,
            'du': None,
            'nc': 'Z74',
            'v': 47160653.0,
            'vl': 13353.0,
            'dpc': None,
            'lf': None,
            'ig': None,
            'ed': None
            },
        ...]
    """
    type = 'intraday' if period == HistoricPeriods.ONE_DAY else 'historic'
    data = self.do_sgx_json_request(
        f"https://api.sgx.com/securities/v1.1/charts/{type}/stocks/code/{stock_code}/{period}"
    )
    return data[type]

  def get_stock_announcements(self, stock_code):
    data = self.do_sgx_json_request(
        'https://api.sgx.com/announcements/v1.0/securitycode',
        params=dict(
            periodstart="20180720_161034",
            cat="ANNC",
            sub="ANNC17",
            value=stock_code,
            pagestart=0,
            pagesize=250
        )
    )
    return data

  def get_general_information(self, ric: str):
    payload = Soap.request_general_information(ric)
    resp = self.do_soap_request(
        "https://apitrkd.trkd-hs.com/apitrkd/api/Fundamentals/Fundamentals.svc",
        payload,
        dict(
            id='public',
            token=self.token
        )
    )
    return Soap.parse_general_information(resp)

  def get_ratios(self, ric: str):
    payload = Soap.request_ratios(ric)
    resp = self.do_soap_request(
        "https://apitrkd.trkd-hs.com/apitrkd/api/Fundamentals/Fundamentals.svc",
        payload,
        dict(
            id='public',
            token=self.token
        )
    )
    return Soap.parse_ratios(resp)

  def get_corporate_info(self, ibm_code: str):
    try:
      data = self.do_sgx_json_request(
          'https://api.sgx.com/corporateinformation/v1.0',
          params=dict(
              ibmcode=ibm_code
          )
      )[0]
      return data
    except:
      raise Exception('Invalid ibmcode')

  def get_corporate_actions(self, ibmcode):
    """
      including dividend info
    """
    try:
      data = self.do_sgx_json_request(
          'https://api.sgx.com/corporateactions/v1.0',
          params=dict(
              pagesize=10,
              pagestart=0,
              ibmcode=ibmcode,
              cat='dividend',
              params=','.join(['id', 'anncType', 'datePaid',
                               'exDate', 'name', 'particulars', 'recDate'])
          )
      )
      # dates are in milliseconds since unix epoch
      return [
          dict(
              url='https://links.sgx.com/1.0.0/corporate-actions/' +
              str(entry['id']),
              **entry
          ) for entry in data
      ]
    except Exception as e:
      print(e)
      raise Exception('Invalid ibmcode')

  def get_financial_statements(self, ric):
    payload = Soap.request_financial_statements(ric)
    resp = self.do_soap_request(
        "https://apitrkd.trkd-hs.com/apitrkd/api/Fundamentals/Fundamentals.svc",
        payload,
        dict(
            id='public',
            token=self.token
        )
    )
    return Soap.parse_financial_statements(resp)

  def get_snapshot_report(self, ric):
    payload = Soap.request_snapshot_report(ric)
    resp = self.do_soap_request(
        "https://apitrkd.trkd-hs.com/apitrkd/api/Fundamentals/Fundamentals.svc",
        payload,
        dict(
            id='public',
            token=self.token
        )
    )
    return Soap.parse_snapshot_report(resp)

  def stock_search(self, stock_code):
    payload = Soap.request_stock_search(stock_code)
    resp = self.do_soap_request(
        "https://apitrkd.trkd-hs.com/apitrkd/api/Search2/Search2.svc",
        payload,
        dict(
            id='public',
            token=self.token
        )
    )
    return Soap.parse_stock_search(resp)

  def get_ric_by_stock_code(self, stock_code):
    return self.stock_search(stock_code)[0]['PrimaryRIC']
