/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.lang.core.dfa

enum class RunnerResult {
    /**
     * Successful completion
     */
    OK,
    /**
     * Method is too complex for analysis
     */
    TOO_COMPLEX,
    /**
     * Cannot analyze (probably method in severely incomplete)
     */
    NOT_APPLICABLE,
    /**
     * Aborted due to some internal error like corrupted stack
     */
    ABORTED
}
