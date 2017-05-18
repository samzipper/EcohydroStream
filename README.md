# EcohydroStream
EcohydroStream is intended to aggregate and store tweets from
the Twitter Streaming API that match keywords related to 
ecohydrology. The goal is to get a general idea into the discourse
surrounding this relatively new and growing discipline.

The initial commit is based on the AgroStream repository
(https://github.com/szipper/AgroStream.git) with only the search
term and SQLite repository locations changed.

The primary work is done by the script, SearchAndStoreTweets.R.
Windows Task Scheduler is used to run this script daily, as described here:
http://stackoverflow.com/questions/2793389/scheduling-r-script
and here (to have task run in background):
http://stackoverflow.com/questions/6568736/how-do-i-set-a-windows-scheduled-task-to-run-in-the-background
