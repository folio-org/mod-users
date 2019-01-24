@Library ('folio_jenkins_shared_libs@FOLIO-1596') _

buildMvn {
  publishModDescriptor = true
  publishAPI = true
  mvnDeploy = true
  runLintRamlCop = true

  doDocker = {
    buildJavaDocker {
      publishMaster = false
      healthChk = true
      healthChkCmd = 'curl -sS --fail -o /dev/null  http://localhost:8081/apidocs/ || exit 1'
    }
  }
}

