using namespace std;

#include <queue>
#include <ctime>
#include <iostream>
#include <vector>

const int PUSH = 1;
const int POP = 2;
const int FRONT = 3;
const int TIMES = 1000;

int main() {

  string str;
  vector<int> commands;

  char c = cin.get();
  while (cin.good()) {
    switch (c) {
      case '[':
        break;
      case ',':case ']':
        switch (str[0]) {
          case 'H':
            commands.push_back(FRONT);
            break;
          case 'T':
            commands.push_back(POP);
            break;
          case 'S':
            commands.push_back(PUSH);
            commands.push_back(str[5] - '0');
            break;
        }
        str = "";
        break;
      default:
        str += c;
        break;
    }
    c = cin.get();
  }

  std::clock_t    start;
	start = std::clock();

  for (int i = 0 ;i < TIMES; i++){
    queue<int> q;
    for (auto begin = commands.begin(); begin != commands.end(); begin++){
      switch (*begin){
        case PUSH:
          begin++;
          q.push(*begin);
          break;
        case POP:
          q.pop();
          break;
        case FRONT:
          q.front();
          break;
      }
    }
    while (!q.empty()) q.pop();
  }

	cout << "Time: " << (clock() - start) / (double)(CLOCKS_PER_SEC / 1000) << " ms" << endl;
	cout << "Avg: " << (clock() - start) / (double)(CLOCKS_PER_SEC / 1000 ) / TIMES << " ms" << endl;
	return 0;
}
