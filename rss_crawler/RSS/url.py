import datetime
import requests

#import from local directory
from hash import sha256

headers = {'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.95 Safari/537.36'}

def timestamp():
    return datetime.datetime.utcnow().strftime('%Y%m%d%H%M%S')

from urllib import urlencode
from urlparse import urlparse, urlunparse, parse_qs
def remove_query(url):
    return urlunparse(urlparse(url)._replace(query=''))

class URL(object):
    def __init__(self, link_type, link):
        self.link =link
        self.link_type = link_type
    def get(self):
        try:
            self.res = requests.get(self.link, headers=headers)
            self.name = "%s.%s.%s"%(self.link_type,timestamp(),sha256(self.link))
        except:
            return False
        return self.res.ok
    def save_to_disk(self, path):        
        with open('%s/%s'%(path, self.name), 'w') as f:
            f.write(self.res.content)

