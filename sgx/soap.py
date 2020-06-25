from functools import reduce


class Soap:
  @staticmethod
  def request_screener_all():
    return """
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
        """

  @staticmethod
  def parse_screener_all(resp):
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

  @staticmethod
  def request_general_information(ric: str):
    return f"""
          <s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:a="http://www.w3.org/2005/08/addressing">
            <s:Header>
              <a:To>http://api.trkd.thomsonreuters.com/api/Fundamentals/Fundamentals.svc</a:To>
              <a:MessageID>TH9uDQHSPTIkOAArSJUYolMaaODMlBRx</a:MessageID>
              <a:Action>http://www.reuters.com/ns/2009/01/26/webservices/rkd/Fundamentals_1/GetGeneralInformation_1</a:Action>
              <Authorization xmlns="http://www.reuters.com/ns/2006/05/01/webservices/rkd/Common_1">
                <ApplicationID></ApplicationID>
                <Token></Token>
              </Authorization>
            </s:Header>
            <s:Body>

          <GetGeneralInformation_Request_1 companyId="{ric}" companyIdType="RIC" countryCode="SG"
            ShowReferenceInformation="false" lang="en-US"
            xmlns:xsd="http://www.w3.org/2001/XMLSchema"
            xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
            xmlns="http://www.reuters.com/ns/2009/01/26/webservices/rkd/Fundamentals_1" />
            </s:Body>
          </s:Envelope>
        """

  @staticmethod
  def parse_general_information(resp):
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

  @staticmethod
  def request_ratios(ric: str):
    return f"""
      <s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:a="http://www.w3.org/2005/08/addressing">
        <s:Header>
          <a:To>http://api.trkd.thomsonreuters.com/api/Fundamentals/Fundamentals.svc</a:To>
          <a:MessageID>UZuCBBqvu9aFU2g6Bs4vfSQ4c1rV0pkF</a:MessageID>
          <a:Action>http://www.reuters.com/ns/2009/01/26/webservices/rkd/Fundamentals_1/GetRatiosReports_1</a:Action>
          <Authorization xmlns="http://www.reuters.com/ns/2006/05/01/webservices/rkd/Common_1">
            <ApplicationID></ApplicationID>
            <Token></Token>
          </Authorization>
        </s:Header>
        <s:Body>
          <GetRatiosReports_Request_1 companyId="{ric}" companyIdType="RIC" countryCode="SG"
            xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
            xmlns="http://www.reuters.com/ns/2009/01/26/webservices/rkd/Fundamentals_1" />
        </s:Body>
      </s:Envelope>
    """

  @staticmethod
  def parse_ratios(resp):
    data = resp['s:Envelope']['s:Body']['GetRatiosReports_Response_1']['FundamentalReports']['ReportRatios']
    issue = data['Issues']['Issue']
    general_info = data['CoGeneralInfo']
    ratios = data['Ratios']
    return dict(
        issues=dict(
            name=issue['@Desc'],
            **{item['@Type']: item['#text'] for item in issue['IssueID']},
            exchange=dict(
                code=issue['Exchange']['@Code'],
                country=issue['Exchange']['@Country'],
                name=issue['Exchange']['#text']
            ),
            mostRecentSplit=dict(
                date=issue['MostRecentSplit']['@Date'],
                value=issue['MostRecentSplit']['#text']
            )
        ),
        generalInfo=dict(
            companyStatus=general_info['CoStatus']['#text'],
            companyType=general_info['CoType']['#text'],
            lastModified=general_info['LastModified'],
            latestAvailableAnnual=general_info['LatestAvailableAnnual'],
            latestAvailableInterim=general_info['LatestAvailableInterim'],
            employees=dict(
                lastUpdated=general_info['Employees']['@LastUpdated'],
                value=general_info['Employees']['#text']
            ),
            sharesOut=dict(
                date=general_info['SharesOut']['@Date'],
                totalFloat=general_info['SharesOut']['@TotalFloat'],
                value=general_info['SharesOut']['#text']
            ),
            reportingCurrency=dict(
                code=general_info['ReportingCurrency']['@Code'],
                value=general_info['ReportingCurrency']['#text']
            )
        ),
        ratios=dict(
            priceCurrency=ratios['@PriceCurrency'],
            reportingCurrency=ratios['@ReportingCurrency'],
            exchangeRate=ratios['@ExchangeRate'],
            date=ratios['@LatestAvailableDate'],
            priceVol=dict(

            ),
            perShare=dict(

            ),
            valuation=dict(

            ),
            financialStrength=dict(

            ),
            incomeStatement=dict(

            ),
            margins=dict(

            ),
            managementEffectiveness=dict(

            ),
            growth=dict(

            )
        )
    )
