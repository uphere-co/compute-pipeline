import requests
from BeautifulSoup import BeautifulSoup, Tag
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

def sha256(link):
    return hashlib.sha256(link).hexdigest()
    
def dump_article(link):
    res=requests.get(link)
    soup = BeautifulSoup(res.content)
    main_elms = [x for x in soup.findAll("p", {"class":"story-body-text story-content"})]
    maintext = '\n\n'.join([para_repr(x) for x in main_elms])
    entities = [y.text for x in main_elms for y in x.findAll("a")]

    summary = soup.find('meta', {'property':'og:description'})['content']
    title = soup.find('meta', {'property':'og:title'})['content']

    uid = sha256(link)
    
    path = '/home/jihuni/word2vec/nyt/'    
    with open(path+uid+'.title', 'w') as f:
        f.write(title.encode('utf-8'))
    with open(path+uid+'.summary', 'w') as f:
        f.write(summary.encode('utf-8'))
    with open(path+uid+'.maintext', 'w') as f:
        f.write(maintext.encode('utf-8'))
    with open(path+uid, 'w') as f:
        f.write(res.content)


link = r.spop('nyt_links')
while(link):    
    if r.sismember('known_nyt_links', link):
        link = r.spop('nyt_links')
        continue
    
    try:        
        dump_article(link)
        logging("Processed %s"%(link))
        r.sadd('processed_nyt_links', link)                
        time.sleep(2)        
    except:
        r.sadd('erratic_nyt_links', link)
        logging("Error to process %s"%(link))
        
    r.sadd('known_nyt_links', link)
    link = r.spop('nyt_links')

