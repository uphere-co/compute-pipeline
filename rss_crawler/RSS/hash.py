import hashlib


def sha256(link):
    return hashlib.sha256(link).hexdigest()

