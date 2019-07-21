#!/usr/bin/env python
import json
import requests
import xmltodict

from constants import HistoricPeriods
from soap_requests import SoapRequests


class SGX:
    def __init__(self):
        self.token = 'nmosUxEP1iCb1HdhRXhQKj9wdTFRdLLD2TP6sKaBkZccGjSvkX9+BRLXbD47kHzl'
        pass

    def do_json_request(self, url, params={}):
        resp = requests.get(url, params=params)
        if resp.status_code != requests.codes.ok:
            raise Exception(f"Http {resp.status_code}: {resp.text}")
        data = resp.json()
        if 'meta' not in data or 'code' not in data['meta'] or data['meta']['code'] != "200":
            raise Exception(f"Returned code {data['meta']['code']}")
        return data

    def do_soap_request(self, url, payload, params={}):
        resp = requests.get(url, params=params, data=payload, headers={
            'Content-Type': 'application/soap+xml'
        })
        if resp.status_code != requests.codes.ok:
            raise Exception(f"Http {resp.status_code}: {resp.text}")
        data = xmltodict.parse(
            resp.content
        )
        return data

    def get_stocks(self, start=0, size=250):
        params = dict(start=start, size=size)
        data = self.do_json_request(
            'https://api.sgx.com/securities/v1.1/gtis',
            params=params
        )
        return data

    def get_all_stocks_names(self):
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
        payload = SoapRequests.make_screener_all_request()
        resp = self.do_soap_request(
            "https://apitrkd.trkd-hs.com/apitrkd/api/Screener/Screener.svc",
            payload,
            params=dict(
                id='public',
                token=self.token
            ),
        )

        attributes = [
            "exchange",
            "exchange_country_code",
            "name",
            "ticker",
            "ric",
            "market_cap",
            "total_revenue",
            "price_to_earnings",
            "yield",
            "4_week_change",
            "13_week_change",
            "26_week_change",
            "52_week_change",
            "net_profit",
            "returns_over_earnings",
            "price_to_cash_flow",
            "debt_over_equity",
            "1_year_revenue_change",
            "sector",
            "currency",
            "price_over_book"
        ]
        raw_data = [
            result['v'] for result in resp['s:Envelope']['s:Body']['Calculate_Response_1']['rs']['r']]
        data = [
            {
                attributes[i]: (v if isinstance(v, str) else None) for i, v in enumerate(stock)
            } for stock in raw_data
        ]
        return data

    def get_basic_info_by_stock(self, stock_code):
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

    def get_historical_data_by_stock(self, stock_code, period=HistoricPeriods.ONE_YEAR):
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

    def get_stock_announcements(self, stock_code):
        data = self.do_json_request(
            'https://api.sgx.com/announcements/v1.0/securitycode',
            params=dict(
                periodstart="20180720_161034",
                cat="ANNC",
                sub="ANNC17",
                value=stock_code,
                pagestart=0,
                pagesize=250
            )
        )['data']
        return data

    def get_general_fundamentals_by_stock(self, RIC):
        payload = SoapRequests.make_general_fundamentals_request(RIC)
        resp = self.do_soap_request(
            "https://apitrkd.trkd-hs.com/apitrkd/api/Fundamentals/Fundamentals.svc",
            payload,
            dict(
                id='public',
                token=self.token
            )
        )
        data = resp['s:Envelope']['s:Body']['GetGeneralInformation_Response_1']['GeneralInformation']
        return dict(
            company_name=data['CompanyName']['#text'],
            industry_classification=[item for sublist in [
                (
                    [v['@Description'] for v in d['Detail']] if isinstance(d['Detail'], list)
                    else [d['Detail']['@Description']]
                ) for d in data['IndustryClassification']['Taxonomy']
            ] for item in sublist],
            employees=data['CompanyGeneralInfo']['Employees']['#text']
        )


if __name__ == "__main__":
    sgx = SGX()
    # print(sgx.get_all_stocks_names())
    # print(sgx.get_basic_info_by_stock('Z74'))
    # print(sgx.get_historical_data_by_stock('Z74', period=HistoricPeriods.ONE_YEAR))
    # print(sgx.get_all_stocks_attributes())
    # print(sgx.get_stock_announcements_by_stock('Z74'))
    print(json.dumps(sgx.get_general_fundamentals_by_stock('STEL.SI'), indent=2))
