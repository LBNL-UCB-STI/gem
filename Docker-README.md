## GEM Docker Image

## Pre-requisites
Install [Docker CE](https://docs.docker.com/install/)

## Building the docker image locally

To build the docker image for the `GEM` project , run the below command from root path of the project

```bash
docker build -t gem:[tag] .
```
where `[tag]` is a placeholder for a required docker tag to be built. Read more about [docker tags](https://docs.docker.com/engine/reference/commandline/tag/)



Eg :
```bash
docker build -t gem:1.0.0 .
```

## Run the docker image locally

To run the docker image locally , run the below command

```bash
docker run -it --name=gems \
           -v <full-path-to-inputs-folder>:/gem-raw-inputs \
           -v <full-path-to-license-file>:/gams/gamslice.txt \
           -e GIT_USERNAME=[username] \
           -e GIT_PASSWORD=[password] \
           -e GIT_REPO_NAME=LBNL-UCB-STI/gem \
           -e INPUT_FILE=[path-to-input-file] \
           gems:[tag]
```
Eg : 
```bash
docker run -it --name=gems \
           -v /home/gem-project/gem-raw-inputs:/gem-raw-inputs \
           -v /home/gem-project/gamslice.txt:/gams/gamslice.txt \
           -e GIT_USERNAME=yourgitusername \
           -e GIT_PASSWORD=gitpassword \
           -e INPUT_FILE="input/experiments/base.yaml" \
           gems:1.0.0
```
> Note that volume mounts requires `full path` of the local folder . In case of improper/failed mount no error is thrown.

> This image also takes optional env variables `GIT_HOST_URL` , `GIT_REPO_NAME` and `COMMIT_ID`. The below default values are picked if none of these optional arguments are passed.
* `GIT_HOST_URL` = github.com; 
* `GIT_REPO_NAME` = LBNL-UCB-STI/gem;
* `COMMIT_ID` = if no commit id then **master** branch is used

## Pushing the image to registry

> Step recommended only after the image built in previous step is well tested.

To push a local image to registry , an authenticated `docker ID` is required . 

#### Login to your docker account , using the below command
```bash
docker login
```
> This is an interactive step , that would ask for a `Username` and `Password`. Upon providing valid credentials the message **Login Succeeded** would be displayed.

#### Tag the local image with your account ID

```bash
docker tag gem:[tag] youraccountID/gem:[tag]
``` 

Push the tagged image to registry
```bash
docker push youraccountID/gem:[tag]
``` 
> This would push the image to the remote docker repo `gem` under the provided `docker account` on cloud.

Eg :
```bash
docker tag gem:1.0.0 youraccountID/gem:1.0.0
docker push youraccountID/gem:1.0.0
```

## Pulling the image from registry

To pull the image from registry , run the below command 

```bash
docker pull youraccountID/gem:[tag]
``` 
Eg:
```bash
docker pull youraccountID/gem:1.0.0
``` 

> Pulling an image from `public repo` does not need docker credentials. But to pull an image from `private repo` docker login is required.