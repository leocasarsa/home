import urllib2
from contextlib import closing
from bs4 import BeautifulSoup

def get_resources_affectiva():
    """ Get all the youtube video addresses for the affectiva
    emotional ai summit.
    """

    address = "http://go.affectiva.com/emotionai-summit-content?submissionGuid=3ca70c23-cf92-4dfc-bb9c-1e1742442494"
    with closing(urllib2.urlopen(address)) as page:
        soup = BeautifulSoup(page)

        youtube_links, dropbox_links = [], []
        for link in soup.find_all('a'):
            url = link.get('href')
            if 'you' in url:
                youtube_links.append(url)
            if 'dropbox' in url:
                dropbox_links.append(url)

    return youtube_links, dropbox_links

def main():
    print get_resources_affectiva()

if __name__ == "__main__":
    main()
