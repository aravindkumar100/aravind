DEFAULT_WAIT_TIME = 10
class logout:
    home_button = '//*[contains(@resource-id,"com.ebay.mobile:id/home")]'
    user_image = '//*[contains(@resource-id,"com.ebay.mobile:id/image_user_profile")]'
    logout = '//*[contains(@text,"Sign out")]'
    logout_popup = '//*[contains(@text,"OK")]'
    def __init__(self,driver):
        self.driver = driver
    def app_logout(self):
        """
        :Step 1: Click on Home button
        :Step 2: Tap on my profile image icon
        :Step 3: Click on Sign out button
        :Step 4: Click on Ok to handle confirm popup
        :return: NONE
        """
        try:
            print "LOG OUT APP IN PROGRESS...."
            elem = self.driver.selectXpath(self.home_button,DEFAULT_WAIT_TIME)
            elem1 = self.driver.selectXpath(self.user_image,DEFAULT_WAIT_TIME)
            elem2 = self.driver.selectXpath(self.logout,DEFAULT_WAIT_TIME)
            elem3 = self.driver.selectXpath(self.logout_popup,DEFAULT_WAIT_TIME)
            if elem3!=None:
                print "Logout successful"
                return True
            else:
                print "Logout Failed"
                return False
        except Exception as e:
            print "Exception occurred in app_logout method"
            return False
