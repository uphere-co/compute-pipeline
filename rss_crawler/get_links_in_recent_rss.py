#! /usr/bin/env nix-shell
#! nix-shell -i python2.7 -p python27Packages.beautifulsoup4 -p python27Packages.requests -p python27Packages.redis

import time

from RSS.logging import logging
from RSS.server import get_redis
from RSS.rss import RSS

r = get_redis()


path = '/home/jihuni/word2vec/rss.dump/'
while(len(r.smembers('recent_rss_dumps'))):
    rss_dump_path = r.spop('recent_rss_dumps')
    try:
        rss = RSS(path+rss_dump_path)
        if(not rss.parse()):
            raise
        links = rss.links
        n=r.sadd('rss_article_links', *links)
        n=r.sadd('recent_rss_article_links', *links)
        logging("Got %d new links from %s"%(n,rss_dump_path))
        r.sadd('recent_rss_dumps:processed',rss_dump_path)
    except:
        r.sadd('recent_rss_dumps:erratic',rss_dump_path)

