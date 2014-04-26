# -*- mode: ruby -*-
# vi: set ft=ruby :

# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  config.vm.box = "parallels/ubuntu-12.04"
  config.vm.provision "shell", path: "setup.sh"
  config.vm.hostname = "hydranode"
  config.vm.define "hydranode" do |hydranode|
  end
  config.vm.provider "parallels" do |v|
    v.name = "hydranode"
    v.memory = 512
    v.cpus = 2
  end
end
