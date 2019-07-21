class SoapRequests:
    @staticmethod
    def make_screener_all_request():
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
    def make_general_fundamentals_request(RIC):
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
              
          <GetGeneralInformation_Request_1 companyId="{RIC}" companyIdType="RIC" countryCode="SG"
            ShowReferenceInformation="false" lang="en-US"
            xmlns:xsd="http://www.w3.org/2001/XMLSchema"
            xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
            xmlns="http://www.reuters.com/ns/2009/01/26/webservices/rkd/Fundamentals_1" />
            </s:Body>
          </s:Envelope>
        """
