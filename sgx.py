#!/usr/bin/env python

import requests
from constants import HistoricPeriods


class SGX:
    def __init__(self):
        pass

    def do_json_request(self, url, params={}):
        resp = requests.get(url, params=params)
        if resp.status_code != requests.codes.ok:
            raise Exception(f"Http {resp.status_code}: {resp.text}")
        data = resp.json()
        if 'meta' not in data or 'code' not in data['meta'] or data['meta']['code'] != "200":
            raise Exception(f"Returned code {data['meta']['code']}")
        return data

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
        data = self.do_json_request(
            'https://api.sgx.com/marketmetadata/v2',
            params=params
        )['data']
        if len(data) != 1:
            raise Exception("Invalid basic info")
        basic_info = data[0]
        return basic_info

    def get_historic_data(self, stock_code, period=HistoricPeriods.ONE_YEAR):
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
        data = self.do_json_request(
            f"https://api.sgx.com/securities/v1.1/charts/{type}/stocks/code/{stock_code}/{period}"
        )
        return data['data'][type]


if __name__ == "__main__":
    sgx = SGX()
    # print(sgx.get_all_stocks())
    # print(sgx.get_basic_info('Z74'))
    # print(sgx.get_historic_data('Z74', period=HistoricPeriods.ONE_YEAR))
