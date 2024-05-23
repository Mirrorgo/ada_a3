# ada assignment 3
## tasks
### task1
测试用例
- (0,0,'A'),(1,2,'B) ✅
- (0,0,‘a’), (2,1,‘b’) ❌
-  (0,2,‘A’), (0,1,‘B’) ✅
## command
```bash
$ gnatmake --version
$ gnatprove --version
$ gnatmake main
```

## tip
To_Big_Integer在prove的时候可以使用
## tasks
> 只需要修改lz77.abd和main.adb,其他文件都不要动

- task1 ✅ 
  - 条件太多，看看能否简化
  - Output_Length的初始化问题，应该是1吧
- task2 ✅ 优化Loop_invariant成为一个等式
- 


## Commit Message Format
- chore：这是一个常见的前缀，用于表示其他类别未涵盖的常规任务
- build: Changes that affect the build system or external dependencies (example scopes: gulp, broccoli, npm)
- ci: Changes to our CI configuration files and scripts (example scopes: Travis, Circle, BrowserStack, SauceLabs)
- docs: Documentation only changes
- feat: A new feature
- fix: A bug fix
- perf: A code change that improves performance
- refactor: A code change that neither fixes a bug nor adds a feature
- style: Changes that do not affect the meaning of the code (white-space, formatting, missing semi-colons, etc)
- test: Adding missing tests or correcting existing tests

## 坑
Output'First <= Output'Last 无法prove
Output'First <= Output'Length + Output'First 却可以prove