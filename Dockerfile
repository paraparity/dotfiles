FROM silex/emacs:26.2

RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
	ledger pandoc \
	&& rm -rf /var/lib/apt/lists/*

ADD .emacs.d /root/.emacs.d
RUN emacs -batch -l ~/.emacs.d/init.el

WORKDIR /root/
CMD "emacs"

# Instead of running emacs immediately, we could also open a shell to invoke
# Emacs multiple times to verify repeat launch behavior
# CMD "bash"
