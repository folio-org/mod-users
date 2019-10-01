@Library ('folio_jenkins_shared_libs@FOLIO-2267') _

buildMvn {
  publishModDescriptor = true
  publishAPI = true
  mvnDeploy = true
  runLintRamlCop = true
  doKubeDeploy = true

  doDocker = {
    buildJavaDocker {
      publishMaster = true
      publishPreview = true
      healthChk = true
      healthChkCmd = 'curl -sS --fail -o /dev/null  http://localhost:8081/apidocs/ || exit 1'
    }
  }
}

