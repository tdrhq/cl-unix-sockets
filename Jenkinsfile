pipeline {
    agent master

    stages {
	    stage ('Checkout') {
            steps {
 	            checkout([$class: 'GitSCM',
                          branches: [[name: (env.DIFF_ID != null ? "phabricator/diff/" + env.DIFF_ID :  (env.BRANCH_NAME ?: "master"))]],
                          doGenerateSubmoduleConfigurations: false,
                          extensions: [[$class: 'SubmoduleOption',
                                        disableSubmodules: false,
                                        parentCredentials: false,
                                        recursiveSubmodules: true,
                                        reference: '',
                                        trackingSubmodules: false]],
                          submoduleCfg: [],
                          userRemoteConfigs: [[url: 'https://github.com/tdrhq/cl-unix-sockets.git']]])
                sh "rm -f .buckconfig.local"
                sh "rm -rf buck-out/logcats/"
                sh "rm -f buck-out/*.txt"
            }
	    }

        stage ('sbcl') {
            sh "sbcl --script test-runner.lisp"
        }

        stage('Lispworks') {
            sh "/opt/software/lispworks/lispworks-* -build test-runner.lisp"
        }
    }

}
