# kubernetes-helm
Emacs utility functions and bindings for [helm](https://helm.sh/), the kubernetes package manager.

## Installation

For now copy and load the `kubernetes-helm.el` file.

## Usage:

Invoke the interactive functions

```
M-x kubernetes-helm-dep-up
M-x kubernetes-helm-install
M-x kubernetes-helm-upgrade
M-x kubernetes-helm-values
M-x kubernetes-helm-status
```

To respectively
- update the dependencies
- install a chart
- upgrade a chart
- get the values of a deployed chart
- get the status of a deployment

Note that in most cases, you will be prompted for the k8s namespace.
