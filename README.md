
# Migration utility from *BitBucket* to *GitLab*

An interactive command line utility designed to help migrate `git` repositories from BitBucket to GitLab.

## Quick start

```bash
$ export BITBUCKET_ACCESS_TOKEN="$(< file_with_bitbucket_access_token)"
$ export GITLAB_ACCESS_TOKEN="$(< file_with_gitlab_access_token)"

$ hmig import --bitbucket-project=rss --gitlab-namespace=subsystems/rss --bitbucket-user=john.connor
...interactive selection of repositories...

$ hmig list --bitbucket-project=rss --gitlab-namespace=subsystems/rss --repo=rss_asn --repo=rss_ibs
RSS_ASN
RSS_IBS

$ hmig clean --bitbucket-project=rss --gitlab-namespace=subsystems/rss
...interactive selection of repositories...
```

## Build / Install

To build the utility you need to install [`stack`](https://docs.haskellstack.org/en/stable/README/)
– the standard build utility for *Haskell*.

Then run the build from the root directory of the repository (the directory where this `README.md` file is located):

```bash
$ stack install
```

After its successful execution, the command-line utility `hmig` will appear in the directory `~/.local/bin`
(in *Windows* it is `%APPDATA%\Roaming\local\bin`).


## Usage

The utility has a built-in help. You can always specify the command line key `--help` to get it:

```bash
$ hmig --help
Migration utility from BitBucket to GitLab. Version 0.2.0.0

Usage: hmig COMMAND [--version]

  An interactive utility designed to help migrate `git` repositories from BitBucket to GitLab.

Available options:
  --version                         Print version info
  -h,--help                         Show this help text

Available commands:
  import                            Interactively imports repositories from the selected BitBucket
                                    project into the selected GitLab namespace. Repositories already
                                    present in the selected GitLab namespace are ignored and are not
                                    offered for import. In this way, you can import only part of the
                                    repositories, and then the remaining ones. Alternatively, you can
                                    make sure that all repositories have already been imported if no
                                    repositories are offered for import. If you need to reimport an
                                    existing repository in GitLab, then you first need to delete it.
                                    This can be done, for example, with the `clean` command of this
                                    utility.
  clean                             Allows you to interactively select to delete GitLab repositories
                                    from the specified namespace that have corresponding repositories
                                    in the specified BitBucket project. This will allow you to
                                    re-import these repositories from BitBucket to GitLab.
  list                              Prints a list of imported (or ready-to-import) repositories.
  post-processing                   Post processing of imported repositories. This command will be
                                    useful if you have already imported some repositories from
                                    BitBucket to GitLab with another utility. Or if during the initial
                                    import using this utility there were not enough permissions to
                                    read the corresponding settings in the BitBucket repository and
                                    the import of these settings was skipped.
```

To get help for a specific command:

```bash
$ hmig import --help
Usage: hmig import (-a|--bitbucket-access-token TOKEN) (-A|--gitlab-access-token TOKEN) 
                   [-u|--bitbucket-url URL] [-U|--gitlab-url URL] (-p|--bitbucket-project PROJECT)
                   (-P|--gitlab-namespace NAMESPACE) [-r|--repo REPO] [-R|--no-repo REPO] 
                   [-f|--file-repo FILE] [-F|--file-no-repo FILE] (-p|--bitbucket-user USER) 
                   [--skip-docpart-just] [--only-docpart-just] [--skip-docpart-merged-as-subtree] 
                   [--only-docpart-merged-as-subtree]

  Interactively imports repositories from the selected BitBucket project into the selected GitLab
  namespace. Repositories already present in the selected GitLab namespace are ignored and are not
  offered for import. In this way, you can import only part of the repositories, and then the
  remaining ones. Alternatively, you can make sure that all repositories have already been imported if
  no repositories are offered for import. If you need to reimport an existing repository in GitLab,
  then you first need to delete it. This can be done, for example, with the `clean` command of this
  utility.

Available options:
  -a,--bitbucket-access-token TOKEN BitBucket access token (default: `BITBUCKET_ACCESS_TOKEN`
                                    environment variable value)
  -A,--gitlab-access-token TOKEN    BitBucket access token (default: `GITLAB_ACCESS_TOKEN` environment
                                    variable value)
  -u,--bitbucket-url URL            BitBucket URL base (default: https://stash.billing.ru)
  -U,--gitlab-url URL               GitLab URL base (default: https://gitlab.nexign.com)
  -p,--bitbucket-project PROJECT    BitBucket project name. Only repositories from this project will
                                    be taken into account
  -P,--gitlab-namespace NAMESPACE   GitLab namespace name. Only repositories from this namespace will
                                    be taken into account
  -r,--repo REPO                    Repository name. Use it if you want to restrict actions to a
                                    specified repository only. This option can be specified several
                                    times to select several repositories
  -R,--no-repo REPO                 Repository name to skip. Use it if you want to ignore this
                                    repository. This option can be specified several times to ignore
                                    several repositories
  -f,--file-repo FILE               File with repository names, one per line. This way you can
                                    restrict actions to specified repositories only. Use as addition
                                    or replacement for `--repo` option
  -F,--file-no-repo FILE            File with repository names to skip, one per line. This way you can
                                    ignore some repositories. Use as addition or replacement for
                                    `--no-repo` option
  -p,--bitbucket-user USER          BitBucket user name. Must correspond to BitBucket access token
                                    (see option `--bitbucket-access-token`)
  --skip-docpart-just               Skip BitBucket repositories whose name ends with `_docpart`, and
                                    there is a corresponding BitBucket repository with the same name,
                                    but without this suffix
  --only-docpart-just               Only BitBucket repositories whose name ends with `_docpart`, and
                                    there is a corresponding BitBucket repository with the same name,
                                    but without this suffix
  --skip-docpart-merged-as-subtree  Skip BitBucket repositories whose name ends with `_docpart`, and
                                    there is a corresponding BitBucket repository with the same name,
                                    but without this suffix, that contains commits from `_docpart`
                                    repository (probably added there by `git subtree`)
  --only-docpart-merged-as-subtree  Only BitBucket repositories whose name ends with `_docpart`, and
                                    there is a corresponding BitBucket repository with the same name,
                                    but without this suffix, that contains commits from `_docpart`
                                    repository (probably added there by `git subtree`)
  -h,--help                         Show this help text
```

### Auto completion

Utility has auto completion feature for all common shells. For example, in *bash* you can prepare it with the next command:

```bash
$ source <(hmig --bash-completion-script `which hmig`)
```

#### Auto completion in *MS Windows*

In order to be able to use the auto completion in *MS Windows*, you can, for example, run the utility in the `git-bash` shell
from the git distribution for *MS Windows*.

Please note that in order for auto completion to work, you must specify the full name of the executable file with the extension:
`hmig.exe`.
