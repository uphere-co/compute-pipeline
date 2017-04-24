## Usage
First, use `nix/shell-python.nix` for experiments. It needs a redis server which is hard-coded to `node1:8937`, for now.
- get_rss_dumps.py : get past snapshots of RSS sites from [Internet Archive](https://www.archive.org/).
- get_links_in_rss.py : get article links from RSS.
- dump_link.py  : get articles from collected links.

### Parse HTML dump files
```
# Two options to parse a HTML dump file, e.g. /opt/NYT.dump/1b3dcb0136c666c070602958b25a357682813c611dd25a653d78c7a12f0762be 
# To Produce following 3 files:
#  NYT.10.1b3dcb0136c666c070602958b25a357682813c611dd25a653d78c7a12f0762be.maintext,
#  NYT.10.1b3dcb0136c666c070602958b25a357682813c611dd25a653d78c7a12f0762be.summary
#  NYT.10.1b3dcb0136c666c070602958b25a357682813c611dd25a653d78c7a12f0762be.title
python ../rss_crawler/parse_article.py NYT "10   /opt/NYT.dump/1b3dcb0136c666c070602958b25a357682813c611dd25a653d78c7a12f0762be" tests/
# Produce 1b3dcb0136c666c070602958b25a357682813c611dd25a653d78c7a12f0762be.tile, and so on:
python ../rss_crawler/parse_article.py NYT /opt/NYT.dump/1b3dcb0136c666c070602958b25a357682813c611dd25a653d78c7a12f0762be tests/
```


## TagSoup
`tagsoup/tagsoup.hs` is a script extracting all the "article:*" meta properties. It saves the result into Json format. For this script, use nix-shell from `tagsoup/shell.nix`

