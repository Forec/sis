# SIS - 工作日志辅助工具

SIS 是一个轻量的个人工作日志辅助工具。

## 安装
### 下载预构建版本
选择并下载 SIS 最新的预构建版本。
* [Windows](#)
* [Linux](#)
* [Mac OS](#)

下载的二进制文件名为 `sis`，将其放置在环境变量 `$PATH$` 包含的目录下。 

### 从源码编译
要从源码编译，你的环境需要 GHC >= 8.0.2。
```shell
git clone https://github.com/Forec/sis.git
cd sis
ghc -O2 sis.hs
```
编译成功将生成二进制文件 `sis`，将其放置在环境变量 `$PATH$` 包含的目录下。 

## 帮助
通过 `sis help` 获取帮助。

## 许可证
此仓库中的全部代码均受仓库中 [License](https://github.com/Forec/sis/blob/master/LICENSE) 声明的许可证保护。
