from POM.UserLoginPage import login
from POM.UserLogoutPage import logout
from testsuite import Testsuite
from POM.SearchandPurchasePage import Searchproduct
from utils.driver import appiumdriver
from robot.libraries.BuiltIn import BuiltIn
from robot.api import logger
from robot.utils.asserts import *
html_pass = '<b style="color:green">PASS</b>'
html_fail = '<b style="color:red">FAIL</b>'

class ebay_shopping_001(object):
    def __init__(self):
        pass
    def testcase001_initialize(self, testname):
        """
        :Step 1 : Function called from Suite_1.text file with Argument Ebayapplication
        :Step 2 : Call Class PRODUCT_VALIDATION with 001 as the testcases ID
        :param testname: Argument PRODUCT_VALIDATION passed from suit_1.text file
        :return: NONE
        """
        robot_env = BuiltIn()
        robot_env.log_to_console("****EXECUTION STARTS HERE****")
        self.tc = globals()[testname]
        self.tc = self.tc("001")
        self.tc.initialize()
    def testcase_setup(self):
        """
        Call Setup method to initialise appium Driver.
        :return: NONE
        """
        self.tc.setup()
    def testcase_test(self):
        """
        Call test method to start the Script execution.
        :return: NONE
        """
        self.tc.test()
    def testcase_cleanup(self):
        """
        Close appium driver
        :return: NONE
        """
        self.tc.cleanup()

class Ebayapplication():
    "Class to run tests on the Ebay app"
    def __init__(self,testid):
        self.testid = testid

    def initialize(self):
        self.objSuite = Testsuite(self.testid)


    def setup(self):
        try:
            if self.testid == "001":
                self.robot_env = BuiltIn()
                self.mydriver = appiumdriver()
                self.login_obj = login(self.mydriver,self.objSuite)
                self.product_obj = Searchproduct(self.mydriver,self.objSuite)
                self.logout_obj = logout(self.mydriver)
                self.robot_env.log_to_console("APPIUM DRIVER INITIALIZED AND EBAY APP OPENED SUCCESSFULLY")
                logger.info("\t <b><h3>APPIUM DRIVER INITIALIZED AND EBAY APP OPENED SUCCESSFULLY : %s </h3></b>" % html_pass,html=True)
                return True
            else:
                return False

        except AssertionError as error:
            logger.info("\t <b><h3>Exception: FAILED TO INTITIALIZE APPIUM DRIVER AND OPEN EBAY OPEN APP : %s </h3></b>" % html_fail,html=True)
            self.robot_env.log_to_console("Exception: FAILED TO INTITIALIZE APPIUM DRIVER AND OPEN EBAY OPEN APP", str(error))
            raise Exception("Exception in setup")
    def test(self):
        "Setup for the test"
        try:
            status = self.login_obj.user_login(self.objSuite.user,self.objSuite.pwd,self.objSuite.apptype)
            print "Login completed successfully",html_pass
            assert_true(status, "Check if user login is successful")
            logger.info("\t <b><h3>USER LOGIN COMPLETED SUCCESSFULLY : %s </h3></b>" % html_pass, html=True)
            self.robot_env.log_to_console("LOGIN COMPLETED SUCCESSFULLY")
        except AssertionError as error:
            self.robot_env.log_to_console("Assersion Error : LOGIN OPERATION FAILED" + str(error))
            logger.info("\t <b><h3>Assersion Error : LOGIN OPERATION FAILED  %s </h3></b>" % html_fail,html=True)
            self.mydriver.capturescreenshot(self.mydriver.driver,"Login failure")
            raise Exception("Exception in login")

        try:
            msg = self.product_obj.search_product(self.objSuite.productname)
            print "Product searched Successfully",html_pass
            assert_true(msg, "Check if product search and purchase is successful")
            logger.info("\t <b><h3>PRODUCT SEARCH AND PURCHASE COMPLETED SUCCESSFULLY : %s </h3></b>" % html_pass, html=True)
            self.robot_env.log_to_console("PRODUCT SEARCHED SUCCESSFULLY")

        except AssertionError as error:
            self.robot_env.log_to_console("Assertion Error : PRODUCT PURCHASE FAILED" + str(error))
            logger.info("\t <b><h3>Assersion Error : PRODUCT PURCHASE FAILED  %s </h3></b>" % html_fail, html=True)
            self.mydriver.capturescreenshot(self.mydriver.driver, "PURCHASE FAILED")
            raise Exception("Exception in product purchase")

        try:
            msg2 = self.logout_obj.app_logout()
            print "App logged out successfully",html_pass
            assert_true(msg2, "Check if product search and purchase is successful")
            logger.info("\t <b><h3>APP LOG OUT COMPLETED SUCCESSFULLY : %s </h3></b>" % html_pass,html=True)
            self.robot_env.log_to_console("APP LOGGED OUT SUCCESSFULLY")

        except AssertionError as error:
            self.robot_env.log_to_console("Assertion Error : APP LOGOUT FAILED" + str(error))
            logger.info("\t <b><h3>Assersion Error : APP LOGOUT FAILED  %s </h3></b>" % html_fail, html=True)
            self.mydriver.capturescreenshot(self.mydriver.driver, "LOGOUT FAILED")
            raise Exception("Exception in app logout")


    def cleanup(self):
        "Tear down the test"
        pass