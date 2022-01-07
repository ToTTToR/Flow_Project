from selenium import webdriver
from selenium.webdriver.common.keys import Keys
import time

driver = webdriver.Firefox()
f = open('pretty_graph')
text = f.read()

driver.get("http://magjac.com/graphviz-visual-editor/")
print(driver.title)

inputtext=driver.find_element_by_class_name('ace_text-input')
inputtext.send_keys(Keys.COMMAND,'a')
inputtext.send_keys(Keys.BACK_SPACE)
time.sleep(1)
inputtext.send_keys(text)

time.sleep(10)
driver.quit()
