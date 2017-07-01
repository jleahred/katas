using System;
using System.Collections.Generic;
using System.Linq;

namespace CodeWarsCSharp
{
    public static class Ensure
    {
        public static bool AreNotNullAndSameLength(IEnumerable<int> list1, IEnumerable<int> list2)
        {
            if (list1 == null || list2 == null)
                return false;
            return list1.Count() == list2.Count(); 
        }
    }

    public class AreTheySame
    {
        public static bool comp(int[] a, int[] b)
        {
            if (Ensure.AreNotNullAndSameLength(a, b))
            {
                var aOrdered = a.Select(x => x*x).OrderBy(x => x);
                var bOrdered = b.OrderBy(x => x);
                return aOrdered.SequenceEqual(bOrdered);
            }
            return false;
        }
    }
}
