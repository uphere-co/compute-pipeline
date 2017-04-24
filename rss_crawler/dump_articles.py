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

all_links = r.smembers('rss_article_links')
r.sadd("articles", *all_links)

link = "dummy"
while(link):
    link = r.spop('articles')
    if r.sismember('articles:downloaded',link):
        print "%s is already downloaded"%link
        continue
    if save_article(link):       
        r.sadd('articles:downloaded',link)
        print "%s is downloaded"%link
    else:
        r.sadd('articles:erratic',link)
        print "%s : failed to download"%link
    time.sleep(1)



