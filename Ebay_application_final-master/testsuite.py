from robot.libraries.BuiltIn import BuiltIn

class Testsuite(object):
    def __init__(self,testcasenum="000"):
        """

        :param testcasenum:
         Initializes the suite by getting all the input data
        """
        robot_env = BuiltIn()
        self.user = robot_env.get_variable_value('${USER_CREDENTIALS.user}')
        self.pwd = robot_env.get_variable_value('${USER_CREDENTIALS.pwd}')
        self.apptype = robot_env.get_variable_value('${APP_TYPE.APP}')
        self.productname = robot_env.get_variable_value('${PRODUCT.NAME}')
        pass
