import time
import requests

from RSS.hash import sha256
from RSS.server import get_redis
from RSS.http import get

r = get_redis()

path = '/home/jihuni/word2vec/article/'
def save_article(link):
    try:
        res = get(link)
        name = sha256(link)
        if not res.ok:
            raise
        with open('%s/%s'%(path, name), 'w') as f:
            f.write(res.content)
    except:
        return False
    return True

all_links = r.smembers('recent_rss_article_links')
r.sadd("recent_articles", *all_links)

link="dummy"
while(link):
    link = r.spop('recent_articles')
    if r.sismember('articles:downloaded',link):
        r.sadd('recent_articles:downloaded',link)
        print "%s is already downloaded previously."%link
        continue
    if r.sismember('recent_articles:downloaded',link):
        print "%s is already downloaded"%link        
        continue
    if save_article(link):       
        r.sadd('articles:downloaded',link)
        r.sadd('recent_articles:downloaded',link)
        print "%s is downloaded"%link
    else:
        r.sadd('articles:erratic',link)
        r.sadd('recent_articles:erratic',link)
        print "%s : failed to download"%link
    time.sleep(1)



