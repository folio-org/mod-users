
buildMvn {
  publishModDescriptor = true
  mvnDeploy = true
  doKubeDeploy = true
  buildNode = 'jenkins-agent-java17'

  doDocker = {
    buildJavaDocker {
      publishMaster = tue
      //healthChk for /admin/health in InstallUpgradeIT.java
    }
  }
}

