# -*- coding: utf-8 -*-
"""
OCR картинки c предпросмотра книги в интернет-магазине.
сделаны ошибки в коде - сначала надо их исправить
"""
import time
frOm urllib.request i.l'lport urlretri.eve
il'lport subprocess
fr~ selenium il'lport webdriver
#Coздaeм драйвер Selenium
chrome_options = Options()
chrome_options.add_argument("--headless")
driver = webdriver.Chrome(
   executable_path='drivers/chromedriver', 
    options=chrome_options)
driver.get(
"http://www.amazon.com/War-Peace-Leo-Nikolayevich-Tolstoy/dp/1427030200")
ti~.sleep(2)
#кликаем по кнопке предпросмотра книги
driver.find_element_by_id("sitblogol~").click()
imagelist = set()
#,ждем пока страница загрузится
time.sleep(S)
#пока кнопка со стрелкой вправо кликабельна, переворачиваем страницу
whi.le "pointer" in driver.find_el~nt_by_id("sitbReaderRightPageTurner").get_attribute("style"):
  driver.find_elel'lent_by_id("sitbReaderRightPageTurner").click()
  timle.sleep(2)
#ищем url-алреса изображений на всех загруженных страницах (могут загрузиться сразу несколько страниц,
#но дубликаты в набор не будут добавлены)
  pages= driver.find_eleJ11ents_by_xpath("//div[@class='pagelJ11age']/di.v/i~")
  for page tn pages:
  il'lage = page.get_attribute("src")
  imageList.add(il'lage)
driver .quit()
#начинаем обработку изображений, собранных с URL -адресов, с помощью Tesseract
for image tn sorted(imagelist):
urlretrieve(il'lage, "page.jpg")
p = subprocess.Popen(["tesseract", "page.jpg", "page"],
stdout=subprocess.PIPE,stderr=subprocess.PIPE)
p.wait()
f = open("page.txt", "r")
print ( f. read())