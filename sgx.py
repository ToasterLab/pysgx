#!/usr/bin/env python
import json
import requests
import xmltodict
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
        payload = '''
            <s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:a="http://www.w3.org/2005/08/addressing">
            <s:Header>
                <a:To>http://api.trkd.thomsonreuters.com/api/Screener/Screener.svc</a:To>
                <a:MessageID>izChnkc4BTDHbKw4jY5X0TREo6BPePOa</a:MessageID>
                <a:Action>http://www.reuters.com/ns/2009/10/01/webservices/rkd/Screener_1/Calculate_1</a:Action>
                <Authorization xmlns="http://www.reuters.com/ns/2006/05/01/webservices/rkd/Common_1">
                <ApplicationID></ApplicationID>
                <Token></Token>
                </Authorization>
            </s:Header>
            <s:Body>
                <Calculate_Request_1 xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                xmlns="http://www.reuters.com/ns/2009/10/01/webservices/rkd/Screener_1">
                <criteria content="ctAll">(is in set({Exchange},[SIN]).AND.{Active}).OR.(is in set({ric},[AVJ.AX,BLTA.JK,0854.HK,TPGC.KL,APW.AX,1145.HK,UOS.AX,0143.HK,IKEN.KL,PRU.L,0255.HK,5344.T,6981.T,8604.T,STA.BK,MSCB.KL,LONN.S,0834.HK,IHHH.KL,1130.HK,0069.HK,8923.T,0693.HK,0903.HK,1298.HK,EMAS.OL]))</criteria>
                <form>
                    <col name="Exchange" />
                    <col name="ExchangeCountryCode" />
                    <col name="Name" />
                    <col name="Ticker" />
                    <col name="RIC" />
                    <col name="MktCap" />
                    <col name="SalesTTM" />
                    <col name="PEExclXorTTM" />
                    <col name="YIELD" />
                    <col name="Pr4W%Chg" />
                    <col name="Pr13W%Chg" />
                    <col name="Pr26W%Chg" />
                    <col name="Pr52W%Chg" />
                    <col name="NPMgn%TTM" />
                    <col name="ROE%TTM" />
                    <col name="Pr2CashFlTTM" />
                    <col name="DbtTot2EqPYQ" />
                    <col name="Sales%ChgTTM" />
                    <col name="SectorDescr" />
                    <col name="PriceCurrCode" />
                    <col name="Pr2BookPQ" />
                    <pos rows="750" row="1" />
                </form>
                </Calculate_Request_1>
            </s:Body>
            </s:Envelope>
        '''
        resp = requests.get(
            "https://apitrkd.trkd-hs.com/apitrkd/api/Screener/Screener.svc",
            params=dict(
                id='public',
                # token might change 🤔
                token='1Le8/lo/ni+HhRttcXs/h+iLkXECxL4HqLUVuGNK2YEvaQjHh8E6+6psij/jP3Nz'
            ),
            data=payload,
            headers={
                'Content-Type': 'application/soap+xml'
            }
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
            result['v'] for result in xmltodict.parse(
                resp.content
            )['s:Envelope']['s:Body']['Calculate_Response_1']['rs']['r']]
        data = [
            {
                attributes[i]: (v if isinstance(v, str) else None) for i, v in enumerate(stock)
            } for stock in raw_data
        ]
        return data

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
    # print(sgx.get_all_stocks_names())
    # print(sgx.get_basic_info('Z74'))
    # print(sgx.get_historic_data('Z74', period=HistoricPeriods.ONE_YEAR))
    # print(sgx.get_all_stocks_attributes())
