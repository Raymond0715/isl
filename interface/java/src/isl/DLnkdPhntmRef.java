package isl;

import java.lang.ref.PhantomReference;
import java.lang.ref.ReferenceQueue;

abstract class DLnkdPhntmRef extends PhantomReference<Object> {

    private DLnkdPhntmRef prv = null;
    private DLnkdPhntmRef nxt = null;

    long ptr = 0;

    public DLnkdPhntmRef(Object referent, ReferenceQueue<? super Object> refQ, long ptr) {
        super(referent, refQ);
        this.ptr = ptr;
    }

    final void insertAfter(DLnkdPhntmRef newPrev) {
        this.prv = newPrev;
        this.nxt = newPrev.nxt;
        this.prv.nxt = this;
        this.nxt.prv = this;
    }

    final void remove() {
        this.prv.nxt = this.nxt;
        this.nxt.prv = this.prv;
        this.nxt = null;
        this.prv = null;
    }

    final DLnkdPhntmRef next() {
        return nxt;
    }

    abstract void freeCPtr();

    static final DLnkdPhntmRef createListDelims() {
        DLnkdPhntmRef start = new DummyDLnkdPhntmRef();
        start.nxt = start;
        start.prv = start;
        return start;
    }

    private static final class DummyDLnkdPhntmRef extends DLnkdPhntmRef {
        private DummyDLnkdPhntmRef() {
            super(new Object(), null, 0L);
        }

        final void freeCPtr() {
            throw new InternalError("Oops, dummy references must not be freed!");
        }
    }
}
