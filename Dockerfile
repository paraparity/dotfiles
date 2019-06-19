FROM silex/emacs:26.2

RUN apt-get update && apt-get install -y ledger

ADD .emacs.d /root/.emacs.d
RUN emacs -batch -l ~/.emacs.d/init.el

WORKDIR /root/
CMD "emacs"
