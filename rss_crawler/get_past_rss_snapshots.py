import time
import requests
from bs4 import BeautifulSoup

from RSS.server import get_redis
from RSS.url import URL

redis = get_redis()

import sys

def logging(line):
    sys.stderr.write(line+'\n')


archive_urls = ["https://web.archive.org/web/20070315000000*/",
                "https://web.archive.org/web/20080315000000*/",
                "https://web.archive.org/web/20090315000000*/",
                "https://web.archive.org/web/20100315000000*/",
                "https://web.archive.org/web/20110315000000*/",
                "https://web.archive.org/web/20120315000000*/",
                "https://web.archive.org/web/20130315000000*/",
                "https://web.archive.org/web/20140315000000*/",
                "https://web.archive.org/web/20150315000000*/",
                "https://web.archive.org/web/20160315000000*/"]

import rss_urls

for url in archive_urls:
    for rss_url, rss_type in rss_urls.rss_urls.iteritems():
        archive = URL("Archive", url + rss_url)
        if(not archive.get()):
            continue;
        archive_soup=BeautifulSoup(archive.res.content, "lxml")
        archive_url = 'https://web.archive.org'
        links = [archive_url+y['href'] for x in archive_soup.findAll("div", {'class':"pop"}) for y in x.findAll('a')]
        for snapshot_url in links:
            time.sleep(1)
            snapshot = URL(rss_type, snapshot_url)
            if(snapshot.get()):
                snapshot.save_to_disk('/home/jihuni/word2vec/rss.dump/')
                redis.sadd('rss_dumps', snapshot.name)
