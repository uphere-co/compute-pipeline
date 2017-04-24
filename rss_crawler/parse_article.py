import requests
from bs4 import BeautifulSoup, Tag
import hashlib
import time
import redis
r = redis.StrictRedis(host='node1', port=8937, db=0)
import sys

def logging(line):
    sys.stderr.write(line+'\n')

def text_repr(elm):
    if type(elm) is Tag:
        return elm.text
    return elm

def para_repr(elm):
    return ''.join([text_repr(x) for x in elm])


def parse_guardian_maintext(soup):
    text=paragraph_join([y for x in soup.findAll('div', {'itemprop':'articleBody'}) for y in x.findAll('p')])
    if text != '':
        return text    
    return paragraph_join(soup.find('div',{'class':'content__standfirst'}).findAll('p'))
    
maintext_parser={}
summary_parser = {}
title_parser = {}
title_parser_default   = lambda soup : soup.find('meta', {'property':'og:title'})['content']
summary_parser_default = lambda soup : soup.find('meta', {'property':'og:description'})['content']
paragraph_join        = lambda xs   : '\n'.join([para_repr(x) for x in xs])


title_parser['NYT']    = title_parser_default
summary_parser['NYT']  = summary_parser_default
maintext_parser['NYT'] = lambda soup : paragraph_join(soup.findAll("p", {"class":"story-body-text story-content"})+
                                                      soup.findAll("p", {'itemprop':'articleBody'}))

title_parser['WP']     = title_parser_default
summary_parser['WP']   = summary_parser_default
maintext_parser['WP'] = lambda soup : paragraph_join([y for x in soup.findAll('article',{'itemprop':"articleBody"}) for y in x.findAll('p')])

title_parser['BBC']    = lambda soup : soup.find('h1', {'class':'story-body__h1'}).text
summary_parser['BBC']  = summary_parser_default
maintext_parser['BBC'] = lambda soup : paragraph_join([y for x in soup.findAll('div', {'class':'story-body__inner'}) for y in x.findAll('p')])

title_parser['CNBC']    = title_parser_default
summary_parser['CNBC']  = summary_parser_default
maintext_parser['CNBC'] = lambda soup : paragraph_join([y for x in soup.findAll('div', {'itemprop':'articleBody'}) for y in x.findAll('p')])

title_parser['Guardian']    = title_parser_default
summary_parser['Guardian']  = summary_parser_default
maintext_parser['Guardian'] = parse_guardian_maintext

class News(object):    
    def __init__(self,full_path, article_type):
        self.name = full_path
        self.type = article_type
        with open(full_path) as f:
            self.soup = BeautifulSoup(f.read(), 'lxml')
    def parse(self):
        try:
            self.maintext = maintext_parser[self.type](self.soup)
            self.summary  = summary_parser[self.type](self.soup)
            self.title    = title_parser[self.type](self.soup)
        except:
            logging("Parse error: %s"%self.name)
            return False
        return True
    def write_to_disk(self, full_path=None):
        if full_path is None:
            full_path = self.name
        with open(full_path+'.title', 'w') as f:
            f.write(self.title.encode('utf-8'))
        with open(full_path+'.summary', 'w') as f:
            f.write(self.summary.encode('utf-8'))
        with open(full_path+'.maintext', 'w') as f:
            f.write(self.maintext.encode('utf-8'))


import os
import sys
if __name__ == '__main__':
    rss_type = sys.argv[1]

    input    = sys.argv[2]
    output   = sys.argv[3]
    output_index = ''
    inputs=input.split()
    if len(inputs)>1:
        output_index,input=inputs
    input_dir, input_name  = os.path.split(input)
    output_dir,output_name = os.path.split(output)
    if output_name=='':
        if output_index == '':
            output_name = input_name
        else:
            output_name = output_index + "." + input_name
    output_fullname = rss_type + "." + output_name
    out_file = os.path.join(output_dir,output_fullname)
    article = News(input, rss_type)
    if(article.parse()):
        article.write_to_disk(out_file)

