from selenium.webdriver.common.by import By
from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.remote.webelement import WebElement
from utils.driver import appiumdriver
DEFAULT_WAIT_TIME = 10
class login:
    login_button = '//*[contains(@resource-id,"com.ebay.mobile:id/button_sign_in")]'
    user_name = '//*[contains(@resource-id,"com.ebay.mobile:id/edit_text_username")]'
    password = '//*[contains(@resource-id,"com.ebay.mobile:id/edit_text_password")]'
    sign_in_button = '//*[contains(@resource-id,"com.ebay.mobile:id/button_sign_in")]'
    no_thanks_opt = '//*[contains(@resource-id,"com.ebay.mobile:id/button_google_deny")]'
    def __init__(self,driver,objsuite):
        self.driver = driver
        self.objSuite = objsuite
        print self.driver


    def user_login(self,user,pwd,app):
        """
            :Step 1: Identify sign in button and click it
            :Step 2: Identify Username field and click on it and finally send the input
            :Step 3: Identify Password field and click on it and finally send the input
            :Step 4: Click on Sign in button
            :Step 4: Click on Nothanks option deny google settings
            :return: NONE
        """
        try:
            if app == 'ebay':
                print "LOG IN TO EBAY APPLICATION IS IN PROGRESS.."
                status = self.driver.selectXpath(self.login_button,DEFAULT_WAIT_TIME)
                print status
                if status!=None:
                    elem = self.driver.selectXpath(self.user_name,DEFAULT_WAIT_TIME)
                    elem.send_keys(user)
                    elem1 = self.driver.selectXpath(self.password,DEFAULT_WAIT_TIME)
                    elem1.send_keys(pwd)
                    if elem and elem1!=None:
                        stat = self.driver.selectXpath(self.sign_in_button,DEFAULT_WAIT_TIME)
                        stat1 = self.driver.selectXpath(self.no_thanks_opt,DEFAULT_WAIT_TIME)
                        if stat and stat1!=None:
                            print "Logged in successfully"
                            return True
                        else:
                            print "Login error"
                            return False
                    else:
                        print "User name or password not entered"
                        return False

                else:
                    print "Sign in button click failed"
                    return False
            elif app =='amazon':
                pass
            elif app =='flipkart':
                pass
            else:
                print "App type not given"
                return False


        except Exception as e:
            print('exception raised in login_ebay function ' )
            print (e)
            return False

