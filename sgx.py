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
        initial_data = self.get_stocks()
        total_pages = initial_data['meta']['totalPages']
        total_data = initial_data['data']
        for i in range(1, total_pages):
            additional_data = self.get_stocks(start=i)['data']
            total_data.extend(additional_data)
        return total_data

    def get_basic_info(self, stock_code):
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
