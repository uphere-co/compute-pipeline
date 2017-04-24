import requests
from bs4 import BeautifulSoup

#import from a local, self directory
from logging import logging
from RSS.url import remove_query

link_tag = {}
link_tag['NYT']      = 'guid'
link_tag['WP']       = 'guid'
link_tag['BBC']      = 'guid'
link_tag['CNBC']     = 'link'
link_tag['Guardian'] = 'guid'
link_tag['LOC']      = 'link'
link_tag['ETUI']     = 'link'
link_tag['ILO']      = 'guid'

class RSS(object):
    def __init__(self,full_path):
        self.name = full_path
        self.type = full_path.split('/')[-1].split('.')[0]
        with open(full_path) as f:
            self.soup = BeautifulSoup(f.read(), 'xml')
    def parse(self):
        try:
            tag = link_tag[self.type]
            self.links = [remove_query(y) for x in self.soup.findAll('item') for y in x.find(tag)]
        except:
            logging("Parse error: %s"%self.name)
            return False
        return True



