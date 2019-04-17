{
 "cells": [
  {
   "cell_type": "code",
   "execution_count": 1,
   "metadata": {},
   "outputs": [
    {
     "ename": "SyntaxError",
     "evalue": "invalid syntax (<ipython-input-1-09ff68888a61>, line 33)",
     "output_type": "error",
     "traceback": [
      "\u001b[0;36m  File \u001b[0;32m\"<ipython-input-1-09ff68888a61>\"\u001b[0;36m, line \u001b[0;32m33\u001b[0m\n\u001b[0;31m    print '%s\\t%s' % (current_word, current_count)\u001b[0m\n\u001b[0m                 ^\u001b[0m\n\u001b[0;31mSyntaxError\u001b[0m\u001b[0;31m:\u001b[0m invalid syntax\n"
     ]
    }
   ],
   "source": [
    "#!/usr/bin/python\n",
    "\n",
    "from operator import itemgetter\n",
    "import sys\n",
    "\n",
    "current_word = None\n",
    "current_count = 0\n",
    "word = None\n",
    "\n",
    "# input comes from STDIN\n",
    "for line in sys.stdin:\n",
    "    # remove leading and trailing whitespace\n",
    "    line = line.strip()\n",
    "\n",
    "    # parse the input we got from mapper.py\n",
    "    word, count = line.split('\\t', 1)\n",
    "\n",
    "    # convert count (currently a string) to int\n",
    "    try:\n",
    "        count = int(count)\n",
    "    except ValueError:\n",
    "        # count was not a number, so silently\n",
    "        # ignore/discard this line\n",
    "        continue\n",
    "\n",
    "    # this IF-switch only works because Hadoop sorts map output\n",
    "    # by key (here: word) before it is passed to the reducer\n",
    "    if current_word == word:\n",
    "        current_count += count\n",
    "    else:\n",
    "        if current_word:\n",
    "            # write result to STDOUT\n",
    "            print '%s\\t%s' % (current_word, current_count)\n",
    "        current_count = count\n",
    "        current_word = word\n",
    "\n",
    "# do not forget to output the last word if needed!\n",
    "if current_word == word:\n",
    "    print '%s\\t%s' % (current_word, current_count)\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": []
  }
 ],
 "metadata": {
  "kernelspec": {
   "display_name": "Python 3",
   "language": "python",
   "name": "python3"
  },
  "language_info": {
   "codemirror_mode": {
    "name": "ipython",
    "version": 3
   },
   "file_extension": ".py",
   "mimetype": "text/x-python",
   "name": "python",
   "nbconvert_exporter": "python",
   "pygments_lexer": "ipython3",
   "version": "3.6.7"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 2
}
