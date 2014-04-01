mysql -u root -h localhost -p
use mysql
CREATE USER 'rpietro'@'localhost' IDENTIFIED BY 'mysql4$';

mysql -u rpietro -p -h localhost test < dump_meta.sql;


cd
cd d2rq-0.8.1\ 2/ 

mv mysql-connector-java-5.1.22-bin.jar mysql-connector.jar 

# drop mysql-connector-java-5.1.28-bin.jar into lib directory
# in order to map has to have primary key
./generate-mapping -o mapping.ttl --tables meta_analysis -u root -p mysql4$ jdbc:mysql://localhost/test

./dump-rdf -f ttl -b http://localhost:2020/ mapping.ttl > ricardo.ttl

sudo ./d2r-server mapping.ttl

sudo ./dump-rdf -u root -p mysql4$ -f n3 -o ricardo.n3 jdbc:mysql://localhost/test

sudo ./d2r-query mapping.ttl "SELECT (avg(?size) as ?avg_size) where { ?x vocab:meta_analysis_sample_size_intervention ?size }"
