import redis

def get_redis():
    return redis.StrictRedis(host='node1', port=8937, db=0)

def remove_set(name):
    while(r.spop(name)):
        pass

def redis_rename_set(r, oldname, newname):
    vals = r.smembers(oldname)
    r.sadd(newname,*vals)
    remove_set(oldname)

