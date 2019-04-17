{
 "cells": [
  {
   "cell_type": "code",
   "execution_count": 1,
   "metadata": {},
   "outputs": [
    {
     "ename": "SyntaxError",
     "evalue": "invalid syntax (<ipython-input-1-06a6a020dd8f>, line 18)",
     "output_type": "error",
     "traceback": [
      "\u001b[0;36m  File \u001b[0;32m\"<ipython-input-1-06a6a020dd8f>\"\u001b[0;36m, line \u001b[0;32m18\u001b[0m\n\u001b[0;31m    print '%s\\t%s' % (word, 1)\u001b[0m\n\u001b[0m                 ^\u001b[0m\n\u001b[0;31mSyntaxError\u001b[0m\u001b[0;31m:\u001b[0m invalid syntax\n"
     ]
    }
   ],
   "source": [
    "#!/usr/bin/python\n",
    "\n",
    "import sys\n",
    "\n",
    "# input comes from STDIN (standard input)\n",
    "for line in sys.stdin:\n",
    "    # remove leading and trailing whitespace\n",
    "    line = line.strip()\n",
    "    # split the line into words\n",
    "    words = line.split()\n",
    "    # increase counters\n",
    "    for word in words:\n",
    "        # write the results to STDOUT (standard output);\n",
    "        # what we output here will be the input for the\n",
    "        # Reduce step, i.e. the input for reducer.py\n",
    "        #\n",
    "        # tab-delimited; the trivial word count is 1\n",
    "        print '%s\\t%s' % (word, 1)\n"
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
