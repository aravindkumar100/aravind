from selenium import webdriver
from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from robot.libraries.BuiltIn import BuiltIn
from robot.api import logger
import os
import logging
import time
class appiumdriver:
    progress_bar = (By.ID, 'progress_bar')
    def __init__(self):
        """
        Appium driver initialization
        """
        robot_env = BuiltIn()
        desired_caps = robot_env.get_variable_value('${DESIRED_CAPS}')
        self.driver = webdriver.Remote('http://127.0.0.1:4723/wd/hub', desired_caps)
        logging.getLogger().setLevel(logging.INFO)
        self.log = logging.getLogger(__name__)
        self.robot_env = BuiltIn()

    def selectXpath(self, xpath, waitSec=10):
        """

        :param xpath: provides the xpath of the particular  to be selected
        :param waitSec: amount of duration to wait
        :return:
        """
        try:
            print "Selecting XPath: {}".format(str(xpath))
            aElem = self.driver.find_element_by_xpath(xpath)
            if (aElem != None):
                aElem.click()
                return aElem
        except:
            return None


    def send_keys(self, locator, key_inputs):
        """
        Typing keys at input locator
        :param locator:
        :param key_inputs:
        """
        try:
            WebDriverWait(self.driver, 20).until(
                EC.visibility_of_element_located(locator)).send_keys(key_inputs)
        except Exception as e:
            print "Exception in send_keys"

    def custom_driver_wait(self, element, timeout=20):
        """
        :param element: Web Element to be validated
        :param timeout: Max time for the element to appear in the Webpage
        :return: Object for the element
        """
        try:
            elem = WebDriverWait(self.driver, timeout).until(EC.visibility_of_element_located((By.XPATH, element)))
            return elem

        except Exception as e:
            self.robot_env.log_to_console("Driver wait failed : " + str(elem), str(e))

    def wait_for_loading(self):
        """

        :return:
        """
        try:
            WebDriverWait(self.driver, 10).until(
                EC.invisibility_of_element_located(self.progress_bar))
        except Exception as e:
            self.log.info("Exception while waiting for the progress bar to disappear " + str(e))


    def click(self, locator, wait_time=10):
        """

        :param locator: element to be clicked
        :param wait_time: wait time duration
        :return:
        """
        try:
            element = WebDriverWait(self.driver, wait_time).until(EC.element_to_be_clickable(locator))
            element.click()
            #self.wait_for_loading()
        except Exception as e:
            self.log.info("Exception while clicking the element " + str(e))


    def capturescreenshot(self, driver, screenname):
        """
        :
        :param driver:
        :param screenname:
        :return:
        """
        try:
            self.foldername = os.getcwd()+"\\..\\..\\ScreenShots"
            screenpath = os.path.join(self.foldername, screenname+".png")
            self.driver.get_screenshot_as_file(screenpath)
            self.robot_env.log_to_console("SCREENSHOT CAPTURED SUCCESSFULLY...")

        except Exception as e:
            self.robot_env.log_to_console("FAIL: SCREENSHOT PROCESS FAILURE DUE TO ISSUES WITH BROWSER...: " +str(e))
            logger.info("FAIL: SCREENSHOT PROCESS FAILURE DUE TO ISSUES WITH BROWSER\n")


    def custom_driver_wait_click(self , element , timeout = 10):
        """

        :param element:
        :param timeout:
        :return:
        """
        try:

            elem = WebDriverWait(self.driver, timeout).until(EC.element_to_be_clickable((By.XPATH, element)))
            self.robot_env.log_to_console("TYPE ELEMENT" + str(elem))
            return elem

        except Exception as e:
            self.robot_env.log_to_console("Driver wait failed: " + str(e))


    def findXpath(self, xpath, waitSec=1):
        """

        :param xpath: xpath of the particular element
        :param waitSec: wait time duration
        :return:
        """
        try:
            print "Finding XPath: {}".format(str(xpath))
            return self.driver.find_element_by_xpath(xpath)
        except:
            return None


