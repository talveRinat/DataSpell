import re
from collections import Counter
import os
import pdfplumber
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np


def prepare_text(text):
    """ Функция подготовки текста к обработке Приводит все слова к нижнему регистру, удаляет все лишние знаки"""

    text = text.lower()
    words = re.split('[^а-я]', text)
    words = list(filter(None, words))
    return words


for file in os.listdir(
        '/Users/talverinat/Documents/Python/Python and data analysis/частота слов '):  # тут указажем путь рабочей среды
    filename = os.fsdecode(file)
    if filename.endswith('.pdf'):
        entire_text = " "
        with pdfplumber.open(file) as pdf:
            for pdf_page in pdf.pages:
                single_page_text = pdf_page.extract_text()
                entire_text += single_page_text

a = prepare_text(entire_text)
n = 30
df = pd.DataFrame(Counter(a).most_common(n), columns=['words', 'freq'])
print(df)


x = [x for x in range(0, n)]
y = df[['freq']]
labels = df['words'].to_list()
plt.plot(x, y)
plt.xticks(x, labels, rotation='vertical')
plt.margins(0.2)
plt.subplots_adjust(bottom=0.15)
plt.grid()
plt.show()

y_pos = np.arange(n)
plt.figure(figsize=(10, 8))
s = 1
expected_zipf = [df.sort_values(by='freq', ascending=False)['freq'][0] / (i + 1) ** s for i in y_pos]
plt.bar(y_pos, df.sort_values(by='freq', ascending=False)['freq'][:30], align='center', alpha=0.5)
plt.plot(y_pos, expected_zipf, color='r', linestyle='--', linewidth=2, alpha=0.5)
plt.xticks(x, labels, rotation='vertical')
plt.ylabel('Частота')
plt.title(f'{n} часто встречаемых слов в книге')
plt.show()
