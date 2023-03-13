#!/usr/bin/env bash

HostName=""
DockerNetworkName=""
DockerPathPrefix=""
Scheme=""
MaxAllowedContainers=""
Admins=""
Devel=""

if [ ! -z "$DEVEL" ]
then
    Devel="--devel=$DEVEL"
fi

if [ ! -z "$HOSTNAME" ]
then
    HostName="--hostname=$HOSTNAME"
fi

if [ ! -z "$SCHEME" ]
then
    Scheme="--scheme=$SCHEME"
fi

if [ ! -z "$ADMINS" ]
then
    Admins="--admins=$ADMINS"
fi

if [ ! -z "$DOCKERNETWORKNAME" ]
then
    DockerNetworkName="--dockernetworkname=$DOCKERNETWORKNAME"
fi

if [ ! -z "$DOCKERPATHPREFIX" ]
then
    DockerPathPrefix="--dockerpathprefix=$DOCKERPATHPREFIX"
fi

if [ ! -z "$MAXALLOWEDCONTAINERS" ]
then
    MaxAllowedContainers="--maxallowedcontainers=$MAXALLOWEDCONTAINERS"
fi

echo $Scheme
echo $HostName
echo $DockerNetworkName
echo $DockerPathPrefix
echo $MaxAllowedContainers
echo $Admins

/bin/dockerproxy-main $Devel $Scheme $HostName $DockerNetworkName $DockerPathPrefix $MaxAllowedContainers $Admins
