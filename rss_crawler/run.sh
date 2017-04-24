cd /home/jihuni/nlp-prototype/rss_crawler/
while :
do
  python dump_recent_rss.py
  python get_links_in_recent_rss.py
  python dump_recent_articles.py
  sleep 43200
done
