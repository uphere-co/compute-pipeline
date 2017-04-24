#! /usr/bin/env nix-shell
#! nix-shell -i python2.7 -p python27Packages.beautifulsoup4 -p python27Packages.requests -p python27Packages.redis
from RSS.server import get_redis
r = get_redis()

redis_sets = [
'articles',                            #This is a buffer list of URLs
'articles:erratic',
'articles:downloaded',
'recent_articles',                     #This is a buffer list of URLs
'recent_articles:erratic',
'recent_articles:downloaded',
'recent_rss_article_links',                 #This is an append-only list of URLs
'recent_rss_article_links:erratic',
'recent_rss_article_links:downloaded',
'rss_article_links',                   #This is an append-only list of URLs
'rss_dumps',
'rss_dumps:processed',
'recent_rss_dumps',
'recent_rss_dumps:erratic',
'recent_rss_dumps:processed']

for x in redis_sets:
    print "%s : %d"%(x,len(r.smembers(x)))

