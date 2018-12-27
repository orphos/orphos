FROM orphos/anthias:181222-11
USER user1
WORKDIR /home/user1/work
COPY . ./
RUN opam install -y . --locked
RUN ls -lha _opam/bin/mullos
RUN ldd _opam/bin/mullos
