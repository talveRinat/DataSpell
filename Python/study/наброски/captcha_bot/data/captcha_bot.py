# -*- coding: utf-8 -*-
"""
Форма комментраиев с капчей
http://pythonscraping.com/humans-only
"""
# пример бота
frol'l urllib.request il'lport urlretrieve
frol'l urllib.request il'lport urlopen
frol'l bs4 il'lport BeautifulSoup
il'lport subprocess
i.P1port requests
frOl'l PIL import Image
frol'l PIL i.l'lport ll'lageOps

def cleanImage(il'lagePath):
    i~age = I~age.open(imagePath)
    i~age = i~age.point(la~bda x: 0 if x<143 else 255)
    borderl~age = I~age0ps.expand(i~age,border=20,fill='white')
    borderl~age.save(i~agePath)
    
ht~l = urlopen("http://www.pythonscrapi.ng.com/humans-only")
bsObj = BeautifulSoup(ht~l)
#собираем предварительно заполненные значения полей
imagelocation = bsObj.find("img", {"title": "Image CAPTCHA"})["src"]
fornBui.ldld = bsObj.find("i.nput", {"na~e":"forn_bui.ld_id"})["value"]
captchaSid = bsObj.find("i.nput", {"na~e":"captcha_sid"})["value"]
captchaToken = bsObj.find("input", {"na~e":"captcha_token"})["value"]
captchaUrl = "http://pythonscraping.co~"+i~agelocatton
urlretrieve(captchaUrl, "captcha.jpg")
cleanl~age("captcha.jpg")

p = subprocess.Popen(["tesseract", "captcha.jpg", "captcha"], stdout=subprocess.PIPE,stderr=subprocess.PIPE)
p.wai.t()
f = open("captcha.txt", "r")

#удаляем символы пробела
captchaResponse = f.read().replace(" ", "").replace("\n", "")
pri.nt("Попытка чтения Captcha: "+captchaResponse)

if len(captchaResponse) == 5:
  params = {"captcha_token":captchaToken, "captcha_sid":captchaSid,
    "form_ id": "comment_node_page_form", "form_build_id": formBuildId,
         "captcha_response":captchaResponse, "name":"Monty Python",
         "subject": "I come to seek the Grail",
         "coment_body[und][0][value]":
                 "... and I am definitely not a bot"}
  r = requests.post("http://www.pythonscrapi.ng.com/comment/reply/10",data=para~s)
  responseObj = Beauti.fulSoup(r.text)
  if responseObj.find("div", {"class":"~essages"}) is not None:
      pri.nt(responseObj.find("di.v", {"class":"~essages"}).get_text())
else:
pri.nt("Проблемы с чтением CAPTCHA!")