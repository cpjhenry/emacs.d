;; https://www.reddit.com/r/emacs/comments/gc4v8f/ewwreadable_like_mode_for_w3m/
;; u/rmurri

;; I currently use python to translate a web page into a readable
;; version for Elfeed. I've considered doing the same for w3m. The
;; library I use is https://github.com/buriy/python-readability/.

(defun elfeed-readability-show-entry (entry)
  "Download readable content from website and show entry in a buffer.
This command is like `elfeed-search-show-entry' but it first downloads the
readable website content if the feed is tagged 'readable'. If the feed is tagged
'excerptReadable' the excerpt is kept above the readable content."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (cond ((elfeed-tagged-p 'excerptReadable entry) (elfeed-readability-content entry t))
        ((elfeed-tagged-p 'readable entry) (elfeed-readability-content entry nil)))
  (elfeed-search-show-entry entry))

(defun elfeed-readability-content (entry &optional keepExcerpt)
  "Replace entry content with readability article.
Some feeds (like heise.de) only provide a summary and not the full article.
This uses a python script to fetch the readable part of the original
article content.  Like in Firefox article view. If keepExcerpt is non nil,
keep the original excerpt above the article."
  (unless (elfeed-meta entry :readability)
    (let* ((url (elfeed-entry-link entry))
           (article (shell-command-to-string (format "python3 '%s' '%s'" elfeed-readability-script url))))
      (message "Downloading article content for: %s" url)
      (if (string-prefix-p "Error fetching " article)
          (message article)
        (progn
          (when keepExcerpt
            (setq article (concat (elfeed-deref (elfeed-entry-content entry)) "<br><br>---<br>" article)))
          (setf (elfeed-entry-content entry)
                (elfeed-ref article))
          (setf (elfeed-meta entry :readability) t))))))

;; python readable_url.py

"""Script to get the readable text from a website.
Like in Firefox article view.
Needs `requests` and `readability-lxml` installed.
"""
import sys
import os
import requests
from readability import Document


def usage(argv):
    cmd = os.path.basename(argv[0])
    print('Usage: %s <url>\n'
          '(Example: "%s https://domain.com/article.html")' % (cmd, cmd))
    sys.exit(1)


if __name__ == '__main__':
    if len(sys.argv) != 2:
        usage(sys.argv)

    url = sys.argv[1]
    try:
        response = requests.get(url)
        if response.ok:
            doc = Document(response.text)
            print(doc.summary())
        else:
            print('Error fetching {}: {}'.format(url, response.reason))
    except:
        print('Error fetching {}: Connection Error'.format(url))

; LocalWords:  rmurri
