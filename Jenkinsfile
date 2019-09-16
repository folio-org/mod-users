
buildMvn {
  publishModDescriptor = true
  publishAPI = true
  mvnDeploy = true
  runLintRamlCop = true
  doKubeDeploy = true

  doDocker = {
    buildJavaDocker {
      publishMaster = tue
      healthChk = true
      healthChkCmd = 'curl -sS --fail -o /dev/null  http://localhost:8081/apidocs/ || exit 1'
    }
  }
}

