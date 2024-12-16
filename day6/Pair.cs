using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace day6
{
    internal class Pair<T>
    {
        private T f, s;
        public Pair(T f, T s)
        {
            this.f = f;
            this.s = s;
        }
        public T First() { return f; }
        public T Second() { return s; }
        public void SetFirst(T x) { f = x; }
        public void SetSecond(T x) { s = x; }
    }
}
