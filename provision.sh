apt-get update
curl -sSL https://get.haskellstack.org/ | sh

iptables -I OUTPUT -d 93.184.216.34 -j DROP -p tcp
echo 1 > /proc/sys/net/ipv4/tcp_syn_retries