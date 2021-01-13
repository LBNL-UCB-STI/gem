# Checkout repository:

git clone https://github.com/LBNL-UCB-STI/gem.git

cd gem

# On Ubuntu install R and other needed dependencies 
sudo apt update
sudo apt install r-base-core
sudo apt-get install libssl-dev
sudo apt install libcurl4-openssl-dev
sudo apt install libxml2-dev
sudo apt install libfontconfig1-dev
sudo apt install libgit2-dev
sudo apt install libgdal-dev
sudo apt install libudunits2-dev

mkdir R_libs

echo "R_LIBS_USER=./R_libs/" >> ~/.Renviron

chmod 544 install.sh

./install.sh

echo 'gem.project.directory = "/home/ubuntu/gem"' >> ~/.Rprofile

# Run test on base
Rscript ./src/gem.R -e input/experiments/base.yaml -t

